package fpinscala.chapter1

object MyModule {
  // Basic
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  // Tail Recursion
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  // Exercise
  def fib1(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, n: Int): Int = {
      if (n == 0) a
      else go(b, a + b, n - 1)
    }

    go(0, 1, n)
  }

  def fib2(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, i: Int): Int = {
      if (i == n) a
      else go(b, a + b, i + 1)
    }

    go(0, 1, 0)
  }

  private def formatAbs(n: Int) = {
    val str = "abs(%d) = %d"
    str.format(n, abs(n))
  }

  private def formatFactorial(n: Int) = {
    val str = "%d! = %d"
    str.format(n, factorial(n))
  }

  // High-order Function
  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val str = "The %s of %d = %d"
    str.format(name, n, f(n))
  }

  // Polymorphic function
  private def findFirst_before(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }

    loop(0)
  }

  private def findFirst_after[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  // Exercise
  private def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n  >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }

  // partial function
  private def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b: B) => f(a, b)
  }

  // currying
  private def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  // uncurrying
  private def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // function composition
  private def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
  }
}
