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
    val expected = true
    val optionMonoid = Monoid.optionMonoid[Int]

    def firstOptionMonoid[A]: Monoid[Option[A]] = Monoid.optionMonoid[A]
    def lastOptionMonoid[A]: Monoid[Option[A]] = Monoid.dual(firstOptionMonoid)

    val op: (Option[Int], Option[Int]) => Option[Int] = optionMonoid.op
    val zero = optionMonoid.zero

    assertResult(expected)(op(aOpt, bOpt) == aOpt)
    assertResult(expected)(op(zero, bOpt) == bOpt)
    assertResult(expected)(op(bOpt, zero) == bOpt)
    assertResult(expected)(op(op(aOpt, zero), bOpt) == aOpt)
    assertResult(expected)(op(aOpt, op(zero, bOpt)) == aOpt)
    assertResult(expected)(op(zero, zero) == zero)
  }

  "foldRight and foldLeft" should "be" in {
    val stringMonoid = Monoid.stringMonoid
    val words = List("Eff", "ect", "ive")
    val expected = "Effective"

    assertResult(expected)(words.foldRight(stringMonoid.zero)(stringMonoid.op))
    assertResult(expected)(words.foldLeft(stringMonoid.zero)(stringMonoid.op))
    assertResult(expected)(Monoid.concatenate(words, stringMonoid))
    assertResult(expected)(Monoid.foldMap(words, stringMonoid)(identity))

    assertResult(expected)(Monoid.foldRight(words)(stringMonoid.zero)(stringMonoid.op))
    assertResult(expected)(Monoid.foldLeft(words)(stringMonoid.zero)(stringMonoid.op))

    val wordSeq = IndexedSeq("Eff", "ect", "ive")
    assertResult(expected)(Monoid.foldMapV(wordSeq, stringMonoid)(identity))
  }

  "A monoid" should "check sequence validation" in {
    val seq = IndexedSeq(4,2,5,1,3)
//    Monoid.foldMapV(seq, Monoid.stringMonoid)
  }
}
