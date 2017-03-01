package fpinscala.datastructures

/**
  * Created by Gnob on 2017. 2. 28..
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], nh: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(nh, xs)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) =>
      if (n == 1) xs
      else drop(xs, n - 1)
  }

  def dropWhile1[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) dropWhile1(xs, f)
      else l
  }

  // currying을 통한 형식 추론
  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile2(xs)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, hs) => Cons(h, init(hs))
  }

  /*
  foldRight(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), 0)(_ + _)
  = 1 + foldRight(Cons(2, Cons(3, Cons(4, Nil))), 0)(_ + _)
  = 1 + (2 + foldRight(Cons(3, Cons(4, Nil)), 0)(_ + _))
  = 1 + (2 + (3 + foldRight(Cons(4, Nil), 0)(_ + _)))
  = 1 + (2 + (3 + (4 + foldRight(Nil, 0)(_ + _))))
  = 1 + (2 + (3 + (4 + 0)))

  foldRight(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Nil: List[Int])((x, y) => Cons(y, x)
  = Cons(1, foldRight(Cons(2, Cons(3, Cons(4, Nil))), 0)(_ + _))
  = Cons(1, Cons(2, foldRight(Cons(3, Cons(4, Nil)), 0)(_ + _)))
  = Cons(1, Cons(2, Cons(3, foldRight(Cons(4, Nil), 0)(_ + _))))
  = Cons(1, Cons(2, Cons(3, Cons(4, foldRight(Nil, 0)(_ + _)))))
  = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
   */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sumByFoldRight(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def productByFoldRight(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def lengthByFoldRight[A](l: List[A]) = foldRight(l, 0)((_, y) => 1 + y)

  def makeList1[A](v: A, n: Int): List[A] = {
    def loop(n: Int): List[A] =
      if (n == 0) Nil
      else Cons(v, loop(n - 1))

    loop(n)
  }

  def append[A](l: List[A], v: List[A]): List[A] = l match {
    case Nil => v
    case Cons(h, tail) => Cons(h, append(tail, v))
  }

  def makeList2[A](v: A, n: Int): List[A] = {
    var a:List[A] = Nil
    for (_ <- 1 to n)
      a = Cons(v, a)
    a
  }

  /* This is wrong foldLeft!
  foldLeft(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), 0)(_ + _)
  = foldLeft(Cons(2, Cons(3, Cons(4, Nil))), 0)(_ + _) + 1
  = (foldLeft(Cons(3, Cons(4, Nil)), 0)(_ + _) + 2) + 1
  = ((foldLeft(Cons(4, Nil), 0)(_ + _) + 3) + 2) + 1
  = (((0 + 4) + 3) + 2) + 1

  foldLeft(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Nil: List[Int])((x, y) => Cons(y, x)
  = Cons(1, foldLeft(Cons(2, Cons(3, Cons(4, Nil))), Nil: List[Int])((x, y) => Cons(y, x))
  = Cons(1, Cons(2, foldLeft(Cons(3, Cons(4, Nil)), Nil: List[Int])((x, y) => Cons(y, x)))
  = Cons(1, Cons(2, Cons(3, foldLeft(Cons(4, Nil), Nil: List[Int])((x, y) => Cons(y, x))))
  = Cons(1, Cons(2, Cons(3, Cons(4, foldLeft(Nil, Nil: List[Int])((x, y) => Cons(y, x)))))
  = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
   */
  def wrongFoldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, tail) => f(wrongFoldLeft(tail, z)(f), h)
  }

  /*
  foldLeft(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), 0)(_ + _)
  = foldLeft(Cons(2, Cons(3, Cons(4, Nil))), 0 + 1)(_ + _)
  = foldLeft(Cons(3, Cons(4, Nil)), (0 + 1) + 2)(_ + _)
  = foldLeft(Cons(4, Nil), ((0 + 1) + 2) + 3)(_ + _)
  = (((0 + 1) + 2) + 3) + 4

  foldLeft(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Nil: List[Int])((x, y) => Cons(y, x)
  = foldLeft(Cons(2, Cons(3, Cons(4, Nil))), Cons(1, Nil))((x, y) => Cons(y, x))
  = foldLeft(Cons(3, Cons(4, Nil)), Cons(2, Cons(1, Nil)))((x, y) => Cons(y, x))
  = foldLeft(Cons(4, Nil), Cons(3, Cons(2, Cons(1, Nil))))((x, y) => Cons(y, x))
  = foldLeft(Nil, Cons(4, Cons(3, Cons(2, Cons(1, Nil)))))((x, y) => Cons(y, x))
  = Cons(4, Cons(3, Cons(2, Cons(1, Nil))))
   */
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, tail) => foldLeft(tail, f(z, h))(f)
  }

  def sumByFoldLeft(ns: List[Int]) = foldLeft(ns, 0)((x, y) => y + x)

  def productByFoldLeft(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def lengthByFoldLeft[A](l: List[A]) = foldLeft(l, 0)((x, _) => x + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))
}