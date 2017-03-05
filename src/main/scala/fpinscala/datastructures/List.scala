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
    var a: List[A] = Nil
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
  def wrongFoldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
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
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, tail) => foldLeft(tail, f(z, h))(f)
  }

  def sumByFoldLeft(ns: List[Int]) = foldLeft(ns, 0)((x, y) => y + x)

  def productByFoldLeft(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def lengthByFoldLeft[A](l: List[A]) = foldLeft(l, 0)((x, _) => x + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

  def appendByFoldRight[A](l: List[A], v: List[A]): List[A] = foldRight(l, v)((x, v) => Cons(x, v))

  def flatten[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(appendByFoldRight)

  def increase(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, tail) => Cons(h + 1, tail))

  def doublesToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, tail) => Cons(h.toString, tail))

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, tail) =>
      if (f(h)) Cons(h, filter(tail)(f))
      else filter(tail)(f)
  }

  def filterByFoldRight[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, tail) =>
      if (f(h)) Cons(h, filterByFoldRight(tail)(f))
      else filterByFoldRight(tail)(f)
    )

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  // 이해하기 힘들다
  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) Cons(x, Nil) else Nil)

  def addZip(l: List[Int], v: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, tail) => v match {
      case Nil => Nil
      case Cons(vh, vtail) => Cons(h + vh, addZip(tail, vtail))
    }
  }

  // 이게 최선인가?
  def zipWith[A](l: List[A], v: List[A])(f: (A, A) => A): List[A] = l match {
    case Nil => Nil
    case Cons(h, tail) => v match {
      case Nil => Nil
      case Cons(vh, vtail) => Cons(f(h, vh), zipWith(tail, vtail)(f))
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def checkSequence(s1: List[A], s2: List[A]): Boolean = s1 match {
      case Nil => true
      case Cons(h1, tail1) => s2 match {
        case Nil => true
        case Cons(h2, tail2) => if (h1 == h2) checkSequence(tail1, tail2) else false
      }
    }

    if (sub == Nil) return false
    else if (lengthByFoldLeft(sup) < lengthByFoldLeft(sub)) return false

    sup match {
      case Nil => false
      case Cons(_, tail) => if (checkSequence(sup, sub)) true else hasSubsequence(tail, sub)
    }
  }
}