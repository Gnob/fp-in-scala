package fpinscala.laziness

/**
  * Stream
  *
  * @author Gnob
  * @since 2017. 03. 06.
  */
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // TODO: implement foldRight()

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n == 0) Empty else Cons(h, () => t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) => if (n == 0) this else t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Empty => false
    case Cons(h, t) => p(h()) || t().exists(p)
  }

  def nonLazyFoldRight[B](x: B)(f: (A,B) => B): B = this match {
    case Empty => x
    case Cons(h, t) => f(h(), t().nonLazyFoldRight(x)(f))
  }

  def foldRight[B](x: => B)(f: (A, => B) => B): B = this match {
    case Empty => x
    case Cons(h, t) => f(h(), t().foldRight(x)(f))
  }

  def notStackSafeExists(p: A => Boolean): Boolean =
    this.foldRight(false)((x, y) => p(x) || y)

  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((x, y) => p(x) && y)

  def takeWhile_1(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((x,y) => if (p(x)) Stream.cons(x, y.takeWhile_1(p)) else Empty)

  def headOption_1: Option[A] =
    foldRight(None: Option[A])((x,_) => Some(x))

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
