package fpinscala.applicative

import fpinscala.applicative.Applicative.Id
import fpinscala.monad.{Functor, Monad}
import fpinscala.state.State

/**
  * Applicative
  *
  * @author SangBong Lee
  * @version 2.0.0
  * @since 2017. 05. 24.
  */

trait Applicative[F[_]] extends Functor[F] {
  // 기본 수단
  def unit[A](A: => A): F[A]
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fa, fab)((a, f) => f(a))

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f): F[A => B])(fa)

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    apply(map(fa)(f.curried): F[B => C])(fb)

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val fab = unit(f.curried)
    apply(apply(apply(fab)(fa))(fb))(fc)
  }

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val fab = unit(f.curried)
    apply(apply(apply(apply(fab)(fa))(fb))(fc))(fd)
  }

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, mbs) => map2(f(a), mbs)(_ :: _))

  def sequence[A](lfa: List[F[A]]): F[List[A]] =
    traverse(lfa)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((_, _))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
      override def map[A, B](fa: (F[A], G[A]))(f: (A) => B): (F[B], G[B]) = ???
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A) = self.unit(G.unit(a))
      def map2[A,B,C](fga: F[G[A]], fgb: F[G[B]])(f: (A,B) => C) =
        self.map2(fga, fgb)(G.map2(_,_)(f))
      override def map[A, B](fa: F[G[A]])(f: (A) => B): F[G[B]] = ???
    }
  }

}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(identity)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

//  def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = {
//    val self = this
//    new Monad[({type f[x] = F[G[x]]})#f] {
//      def unit[A](a: => A) = self.unit(G.unit(a))
//      def flatMap[A,B](mna: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
//        self.flatMap(mna)(na => G.flatMap(na)(a => join(f(a))))
//    }
//  }
}

//trait Traverse[F[_]] {
//  def traverse[G[_]:Applicative, A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
//    sequence(map(fa)(f))
//  def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] =
//    traverse(fga)(ga => ga)
//}


trait Traverse[F[_]] {
  def traverse[G[_]:Applicative, A,B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]:Applicative,A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id,A,B](fa)(f)

  def traverseS[S,A,B](fa: F[A])(f: A => State[S,B]): State[S,F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def zipWithIndex[A](ta: F[A]): F[(A,Int)] =
    traverseS(ta)((a: A) => for {
      i: Int <- State.get[Int]
      _ <- State.set(i + 1)
    } yield (a, i)).run(0)._1
}

object Applicative {

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  val streamApplicative = new Applicative[Stream] {
    // 기본 수단
    override def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    override def map2[A,B,C](a: Stream[A], b: Stream[B])(f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  val streamMonad = new Monad[Stream] {
    // 기본 수단
    override def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    override def flatMap[A,B](fa: Stream[A])(f: A => Stream[B]): Stream[B] =
      fa.flatMap(f)
  }

  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A,B](eea: Either[E, A])(f: A => Either[E, B]) = eea match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }
    }

}