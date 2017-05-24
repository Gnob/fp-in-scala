package fpinscala.applicative

import fpinscala.test.UnitSpec

/**
  * ApplicativeSpec
  *
  * @author SangBong Lee
  * @version 2.0.0
  * @since 2017. 05. 24.
  */
class ApplicativeSpec extends UnitSpec {
  "Monad" should "not define" in {
    val F = Applicative.eitherMonad[String]

    F.map3(Right("a"), Right("b"), Right("c"))((_,_,_))
  }
}
