package fpinscala.laziness

import fpinscala.test.UnitSpec

/**
  * LazinessIntro
  *
  * @author Gnob
  * @since 2017. 03. 06.
  */
class LazinessIntro extends UnitSpec {
  behavior of "laziness"

  "They" should "not be lazy" in {
    println(List(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).map(_ * 3))

    def square(x: Double): Double = x * x

    assert(square(41.0 + 1.0) == 42.0 * 42.0)
    assertThrows[Exception] { square(sys.error("failure")) }
  }

  "This" should "be lazy" in {
    assert((false && { println("!!"); true }) == false)
    assert((true || { println("!!"); false }) == true)
  }

  "if()" should "use thunk" in {
    def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
      if (cond) onTrue() else onFalse()

    val a: Int = 11

    if2(a < 22,
      () => println("a"),
      () => println("b")
    )

    def if3[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
      if (cond) onTrue else onFalse

    if3(false,
      sys.error("fail"),
      println(3)
    )
  }

  "The default laziness" should "be not cached" in {
    def maybeTwice(b: Boolean, i: => Int) = if (b) i+i else 0

    println(maybeTwice(true, { println("hi"); 1+41 }))

    def maybeTwiceCached(b: Boolean, i: => Int) = {
      lazy val j = i
      if (b) j + j else 0
    }

    println(maybeTwiceCached(true, { println("hi"); 1+41 }))
  }
}
