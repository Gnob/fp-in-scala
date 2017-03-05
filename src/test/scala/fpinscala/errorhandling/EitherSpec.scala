package fpinscala.errorhandling

import fpinscala.test.UnitSpec


/**
  * Created by Gnob on 2017. 3. 6..
  */
class EitherSpec extends UnitSpec {
  behavior of "Either"

  "Examples" should "be done well" in {
    assert(EitherExample.mean(IndexedSeq(1.0, 2.0, 3.0)) == Right(2.0))
    assert(EitherExample.mean(IndexedSeq()).isInstanceOf[Left[String,_]])

    assert(EitherExample.safeDiv(1, 2) == Right(0))
    assert(EitherExample.safeDiv(1, 0).isInstanceOf[Left[Exception,_]])
  }

}
