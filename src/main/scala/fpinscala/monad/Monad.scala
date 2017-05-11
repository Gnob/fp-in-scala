package fpinscala.monad

import fpinscala.testing.Gen

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

  def unzip[A,B] = distribute[A,B]

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A,B]] =
    e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fa) => map(fa)(Right(_))
    }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = la match {
    case Nil => unit(Nil)
    case h :: t =>  flatMap(f(h))(b => map(traverse(t)(f))(b +: _))
  }

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(identity)
}

object Monad {
  val listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
  }

  val genMonad = new Monad[Gen] {
    override def unit[A](a: A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](fa: Gen[A])(f: (A) => Gen[B]): Gen[B] = fa flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: A): List[A] = List(a)
    override def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] = fa flatMap f
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: A): Option[A] = Some(a)
    override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa flatMap f
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: A): Stream[A] = Stream(a)
    override def flatMap[A, B](fa: Stream[A])(f: (A) => Stream[B]): Stream[B] = fa flatMap f
  }
}
