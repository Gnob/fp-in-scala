package fpinscala.chapter1

import fpinscala.test.UnitSpec
import org.scalatest._

/**
  * Created by Gnob on 2017. 2. 28..
  */
class Chapter1Spec extends UnitSpec {
  "abs()" should "make any value positive" in {
    assert(MyModule.abs(3) == 3)
    assert(MyModule.abs(-3) == 3)
  }

  "factorial()" should "calculate correctly" in {
    assert(MyModule.factorial(5) == 120)
    assert(MyModule.factorial(11) == 39916800)
  }

  "fib()" should "calculate correctly" in {
    assert(MyModule.fib1(20) == 6765)
    assert(MyModule.fib2(20) == 6765)
  }

  /*
  println(formatResult("absolute value", -3, abs))
  println(formatResult("factorial", 5, factorial))
  println(formatResult("fibonacci 1", 20, fib1))
  println(formatResult("fibonacci 2", 20, fib2))

  val strArray = Array("absolute value", "factorial", "fibonacci")
  println(findFirst_before(strArray, "factorial"))
  println(findFirst_after[String](strArray, (s: String) => s == "factorial"))

  val intArray = Array(11, 22, 42)
  println(findFirst_after[Int](intArray, (x: Int) => x == 42))

  println(isSorted[Int](intArray, (x: Int, y: Int) => x < y))
  println(isSorted[Int](intArray, (x: Int, y: Int) => x > y))
  */
}
