package fpinscala.errorhandling

import fpinscala.datastructures.{Cons, List, Nil}

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

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    if (map(f) getOrElse false) this
    else None

  // Another Answer
  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  /*
  def wrongMap2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    val va = a.getOrElse(None)
    val vb = b.getOrElse(None)
    if (va == None || vb == None) None
    Some(f(va, vb))
  }
  */

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  /*
  def wrongSequence[A](a: List[Option[A]]): Option[List[A]] =
    Try(List.map(a)(aa => aa getOrElse(throw Exception)))
  */

  def inefficientSequence[A](a: List[Option[A]]): Option[List[A]] = {
    def go(as: List[Option[A]]): List[A] = as match {
      case Nil => Nil
      case Cons(h, tail) => h match {
        case None => Nil
        case Some(x) => Cons(x, go(tail))
      }
    }

    val res: List[A] = go(a)
    if (List.lengthByFoldLeft(a) == List.lengthByFoldLeft(res)) Some(res)
    else None
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case Cons(h, tail) => h flatMap(hh => sequence_1(tail) map (Cons(hh, _)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)

  def inefficientTraverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence_1(List.map(as)(f))

  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case Nil => Some(Nil)
    case Cons(h, tail) => f(h) flatMap(hh => traverse(tail)(f) map (Cons(hh,_)))
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }
}

object OptionExercise {
  def variance(xs: Seq[Double]): Option[Double] = ErrorHandling.mean2(xs).flatMap(m => ErrorHandling.mean2(xs.map(x => math.pow(x - m, 2))))
}

object OptionExample {
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    numberOfSpeedingTickets / age

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Option.Try(age.toInt)
    val optTickets: Option[Int] = Option.Try(numberOfSpeedingTickets.toInt)

    // Option을 사용하고 싶지만 애초에 옵션 값을 받는 함수가 아님
    // insuranceRateQuote(optAge, optTickets)

    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }

  def parseInts(as: List[String]): Option[List[Int]] =
    Option.traverse(as)(x => Option.Try(x.toInt))
}
