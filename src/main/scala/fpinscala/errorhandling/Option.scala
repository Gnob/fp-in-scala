package fpinscala.errorhandling

/**
  * Option
  *
  * @author Gnob
  * @since 2017. 03. 02.
  */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    if (map(f) getOrElse false) this
    else None
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object OptionExercise {
  def variance(xs: Seq[Double]): Option[Double] = ErrorHandling.mean2(xs).flatMap(m => ErrorHandling.mean2(xs.map(x => math.pow(x - m, 2))))
}
