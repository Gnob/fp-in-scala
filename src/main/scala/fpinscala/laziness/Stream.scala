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

  def notStackSafeExists(p: A => Boolean): Boolean = foldRight(false)((x, y) => p(x) || y)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((x, y) => p(x) && y)

  def takeWhile_1(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((x,y) => if (p(x)) Stream.cons(x, y) else Empty)

  def headOption_1: Option[A] = foldRight(None: Option[A])((x,_) => Some(x))

  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((x,y) => Stream.cons(f(x), y))

  def filter(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((x,y) => if (p(x)) Stream.cons(x, y) else y)

  def append[AA >: A](xs: => Stream[AA]): Stream[AA] = foldRight(xs)((x,y) => Stream.cons(x, y))

  def flatMap[B](p: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((x,y) => p(x).append(y))

  def find(p: A => Boolean): Option[A] = filter(p).headOption_1

  def map_1[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  }

  def take_1(n: Int): Stream[A] = Stream.unfold((this, n))(s => s._1 match {
    case Empty => None
    case Cons(h, t) => if (s._2 == 0) None else Some((h(), (t(), s._2 - 1)))
  })

  def takeWhile_2(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Empty => None
    case Cons(h, t) => if (p(h())) Some(h(), t()) else None
  }

  // 이게 최선인가?
  def zipWith[B,C](v: Stream[B])(f: (A,B) => C): Stream[C] =
    Stream.unfold((this, v))(s => s._1 match {
      case Empty => None
      case Cons(h, t) => s._2 match {
        case Empty => None
        case Cons(vh, vt) => Some(f(h(), vh()), (t(), vt()))
      }
    })

  // 이게 최선인가?
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2))(s => s._1 match {
      case Empty => s._2 match {
        case Empty => None
        case Cons(vh, vt) => Some((None, Some(vh())), (Empty, vt()))
      }
      case Cons(h, t) => s._2 match {
        case Empty => Some((Some(h()), None), (t(), Empty))
        case Cons(vh, vt) => Some((Some(h()), Some(vh())), (t(), vt()))
      }
    })

//  def startWith[A](s: Stream[A]): Boolean =
//    zipAll(s).takeWhile_2(x => x._2.isDefined).forAll(x => x._1 == x._2)
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

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def fib(x: Int, nx: Int): Stream[Int] = Stream.cons(x, fib(nx, x + nx))

    fib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some(x) => Stream.cons(x._1, unfold(x._2)(f))
  }

  def constant_1[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  def from_1(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def fibs_1(): Stream[Int] = unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))
}
