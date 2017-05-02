package fpinscala.testing

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import fpinscala.test.UnitSpec

/**
  * GenSpec
  *
  * @author SangBong Lee
  * @version 2.0.0
  * @since 2017. 04. 13.
  */
class GenSpec extends UnitSpec {
  behavior of "Using Gen"

  "List.max()" should "be satisfied for lists except empty list" in {
    val smallInt = Gen.choose(-10,10)
    val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    Prop.run(maxProp)
  }

  "Par" should "be map(unit(1))(_ + 1) == unit(2)" in {
    // 이 속성을 검사해보자
    Par.map(Par.unit(1))(_ + 1) == Par.unit(2)

    val prop = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
      Par.map(i)(_ + 1) == Par.unit(2))

    Prop.run(prop)
    // 위 검사는 실패한다

    // 우리가 기대하는 동등하다는 의미는 임의의 유효한 ExecutorService에 대해
    // 두 Par 객체의 Future 결과가 같다는걸 동등하다고 말하기로 했었다 (숨겨진 가정)
  }

  it should "be" in {
    // 다시 속성을 검사해보자
    Par.map(Par.unit(1))(_ + 1) == Par.unit(2)

    val ES: ExecutorService = Executors.newCachedThreadPool
    val prop = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
      Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

    Prop.run(prop)
  }

  it should "be checked only once" in {
    val ES: ExecutorService = Executors.newCachedThreadPool
    val prop = Prop.check {
      val p = Par.map(Par.unit(1))(_ + 1)
      val p2 = Par.unit(2)
      p(ES).get == p2(ES).get
    }

    Prop.run(prop)
  }

  it should "be clear" in {
    val ES: ExecutorService = Executors.newCachedThreadPool
    val prop = Prop.check {
      Par.equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )(ES).get
    }

    Prop.run(prop)
  }

  "checkPar" should "be" in {
    val S = Gen.weighted(
      Gen.choose(1,4).map(Executors.newFixedThreadPool) -> .75,
      Gen.unit(Executors.newCachedThreadPool) -> .25
    )
    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
      Prop.forAll(S ** g) { case s ** a => f(a)(s).get }

    def checkPar(p: Par[Boolean]): Prop =
      forAllPar(Gen.unit(()))(_ => p)

    object ** {
      def unapply[A,B](p: (A,B)) = Some(p)
    }

    val prop = checkPar {
      Par.equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )
    }

    Prop.run(prop)
  }
}
