package fpinscala.monoid

import fpinscala.test.UnitSpec

/**
  * MonoidSpec
  *
  * @author SangBong Lee
  * @version 2.0.0
  * @since 2017. 05. 08.
  */
class MonoidSpec extends UnitSpec {
  val aOpt = Some(1)
  val bOpt = Some(2)

  "Option Monoid" should "be" in {
    val op: (Option[Int], Option[Int]) => Option[Int] = Monoid.optionMonoid.op
    val zero = Monoid.optionMonoid.zero
    assertResult(true)(op(aOpt, bOpt) == aOpt)
    assertResult(true)(op(zero, bOpt) == bOpt)
    assertResult(true)(op(bOpt, zero) == bOpt)
    assertResult(true)(op(op(aOpt, zero), bOpt) == aOpt)
    assertResult(true)(op(aOpt, op(zero, bOpt)) == aOpt)
    assertResult(true)(op(zero, zero) == zero)
  }
}
