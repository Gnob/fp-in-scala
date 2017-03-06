package fpinscala.errorhandling

import fpinscala.test.UnitSpec
import fpinscala.datastructures.List

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

  "The some functions" should "promote any function" in {
    val absO: Option[Double] => Option[Double] = Option.lift(math.abs)
    assert(absO(Some(-0.1)) == Some(0.1))
    assert(absO(None) == None)

    assert(OptionExample.parseInsuranceRateQuote("10", "40") == Some(4))
    assert(OptionExample.parseInsuranceRateQuote("x", "4") == None)
  }

  "The sequence function" should "perform well" in {
    val longList: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    val wrongList: List[Option[Int]] = List(Some(1), None, Some(3))
    assert(Option.sequence(longList) == Some(List(1, 2, 3)))
    assert(Option.sequence(wrongList) == None)
  }

  "The parseInt function" should "perform well" in {
    val longList = List("1", "2", "3", "4")
    val wrongList = List("1", "2", "x", "4")

    assert(OptionExample.parseInts(longList) == Some(List(1, 2, 3, 4)))
    assert(OptionExample.parseInts(wrongList) == None)
  }
}
