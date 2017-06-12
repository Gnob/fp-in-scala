package fpinscala.iomonad

import fpinscala.monad.Monad

/**
  * Created by Gnob on 2017. 6. 13..
  */
trait IO[A] { self =>
  def run: A

  def map[B](f: A => B): IO[B] =
    new IO[B] { def run = f(self.run) }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] { def run = f(self.run).run }
}


object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] { def run: A = a }
  def flatMap[A,B](fa: IO[A])(a: A => IO[B]): IO[B] = fa flatMap a
  def apply[A](a: => A): IO[A] = unit(a)

  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }
  def ReadLine(): IO[String] = IO { readLine }

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  def tryPureConverter: IO[Unit] = {
    val prompt: IO[Unit] = PrintLine("Enter a temperature in degrees Fahrenheit: ")
    ???
  }

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  val echo = ReadLine.flatMap(PrintLine)
  val readInt = ReadLine.map(_.toInt)
  val readInts = map2(readInt, readInt)((_,_))
  val readLines = replicateM(10, ReadLine)


  case class Player(name: String, score: Int)

  def contest(p1: Player, p2: Player): IO[Unit] =
    PrintLine(winnerMsg(winner(p1, p2)))

  private def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is the winner!"
  } getOrElse "It's draw."

  private def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None
}
