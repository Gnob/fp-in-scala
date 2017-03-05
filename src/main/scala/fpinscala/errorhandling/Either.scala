package fpinscala.errorhandling

/**
  * Created by Gnob on 2017. 3. 6..
  */
sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E,B] = this match {
    case Right(a) => Right(f(a))
    case _ => _
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE,B] = this map(f) match {
    case Right(a) => a
    case Left(e) => Left(e)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE,B]): Either[EE,B] = this match {
    case Left(_) => b
    case _ => _
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE,C] =
    this flatMap(a => b map(bb => f(a, bb)))
}

case class Left[E,A](value: E) extends Either[E, Nothing]
case class Right[E,A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h :: t => f(h) flatMap(b => traverse(t)(f).map(b +: _))
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)
}

object EitherExample {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    Either.Try(x / y)
}
