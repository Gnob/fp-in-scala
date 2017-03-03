package fpinscala.errorhandling

import fpinscala.test.UnitSpec

/**
  * FailFnSpec
  *
  * @author Gnob
  * @version 2.0.0
  * @since 2017. 03. 02.
  */
class OptionSpec extends UnitSpec {
  "This function" should "fail" in {
    assertThrows[Exception] { ErrorHandling.failingFn(1) }
  }

  "This function" should "not fail" in {
    assertResult(43) { ErrorHandling.failingFn2(1) }
  }

  "The mean function" should "handle empty seq" in {
    val doubleSeq = Seq(1.0, -1.0)
    val emptySeq: Seq[Double] = Nil

    // How to check and determine that input seq is empty?
    assert(ErrorHandling.mean1(doubleSeq, 1.0) == ErrorHandling.mean1(emptySeq, 0.0))

    assert(ErrorHandling.mean2(doubleSeq) != ErrorHandling.mean2(emptySeq))
    assert(ErrorHandling.mean2(doubleSeq) == Some(0.0))
    assert(ErrorHandling.mean2(emptySeq) == None)
  }

  "The Option" should "perform some method" in {
    val noneOption: Option[Int] = None
    val intOption = Some(1)
    val doubleOption: Option[Double] = Some(1.0)

    assert(intOption.map(1.0 * _) == doubleOption)

    assert(intOption.getOrElse(0) == 1)
    assert(noneOption.getOrElse(0) == 0)
    assert(intOption.flatMap(x => Some(1.0 * x)) == doubleOption)

    assert(intOption.orElse(doubleOption) == intOption)
    assert(noneOption.orElse(doubleOption) == doubleOption)

    assert(intOption.filter(_ == 1) == intOption)
    assert(intOption.filter(_ != 1) == None)
  }

  "Employees" should "find an employee without any error" in {
    // 오류에 대한 처리를 나중으로 미룰 수 있다
    assert(Employees.lookupByName("Joe").map(_.department).getOrElse(None) == "Dev")
    assert(Employees.lookupByName("Joel").map(_.department).getOrElse(None) == None)
  }

  "The variance" should "perform" in {
    assert(OptionExercise.variance(Seq(1.0, 2.0, 3.0, 4.0)) == Some(1.25))
  }
}
