package fpinscala.parsing

import scala.language.{higherKinds, implicitConversions}

/**
  * Created by Gnob on 2017. 4. 24..
  */
trait Parser[ParseError, Parser[+_]] { self =>
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def char(c: Char): Parser[Char]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String]
  = ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
  }
}
