package fpinscala.monad

import fpinscala.applicative.Applicative
import fpinscala.state.State
import fpinscala.testing.Gen

import scala.language.reflectiveCalls

/**
  * Monad
  *
  * @author SangBong Lee
  * @version 2.0.0
  * @since 2017. 05. 11.
  */

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

//  def unzip[A,B] = distribute[A,B]

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A,B]] =
    e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fa) => map(fa)(Right(_))
    }
}

trait Monad[F[_]] extends Functor[F] with Applicative[F]{
  def unit[A](a: => A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))
  override def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))

  override def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = la match {
    case Nil => unit(Nil)
    case h :: t =>  flatMap(f(h))(b => map(traverse(t)(f))(b +: _))
  }

  override def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(identity)

  override def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  override def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def myFilterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    map(product(traverse(ms)(unit(_)), traverse(ms)(f))) {
      x => {
        x._1.zip(x._2).filter(_._2).unzip._1
      }
    }
  }

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def myFlatMap[A,B](m: F[A])(f: A => F[B]): F[B] =
    compose(identity[F[A]], f)(m)

  def _flatMap[A,B](m: F[A])(f: A => F[B]): F[B] =
    compose((_:Unit) => m, f)(())

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List[A]()))((x,xs) =>
      compose(f, (b: Boolean) => if (b) map2(unit(x), xs)(_ :: _) else xs)(x))

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)

  def __flatMap[A,B](m: F[A])(f: A => F[B]): F[B] =
    join(map(m)(f))

  def __compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))

  def doWhile[A](a: F[A])(cond: A => F[Boolean]): F[Unit] = for {
    a1 <- a
    ok <- cond(a1)
    _ <- if (ok) doWhile(a)(cond) else unit(())
  } yield ()

  def forever[A,B](a: F[A]): F[B] = {
    lazy val t: F[B] = forever(a)
    flatMap(a)(_ => t)
  }

  def foldM[A,B](l: Stream[A])(z: B)(f: (B,A) => F[B]): F[B] =
    l match {
      case h #:: t => flatMap(f(z,h))(z2 => foldM(t)(z2)(f))
      case _ => unit(z)
    }

  def foldM_[A,B](l: Stream[A])(z: B)(f: (B,A) => F[B]): F[B] =
    ???
//    skip { foldM(l)(z)(f) }

  def foreachM[A](l: Stream[A])(f: A => F[Unit]): F[Unit] =
    ???
//    foldM_(l)(())((u,a) => skip(fa))
}

case class Id[A](value: A) {
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  def map[B](f: A => B): Id[B] = Id(f(value))
}

object Monad {
  val listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
  }

  val genMonad = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](fa: Gen[A])(f: (A) => Gen[B]): Gen[B] = fa flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] = fa flatMap f
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa flatMap f
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A, B](fa: Stream[A])(f: (A) => Stream[B]): Stream[B] = fa flatMap f
  }

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = fa flatMap f
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] = fa flatMap f
  }
}

object IntStateMonad extends Monad[({type IntState[A] = State[Int,A]})#IntState] {
  override def unit[A](a: => A): State[Int,A] = State(s => (a, s))
  override def flatMap[A, B](fa: State[Int,A])(f: (A) => State[Int,B]): State[Int,B] = fa flatMap f
}
