package fpinscala.state

import fpinscala.state.RNG.Rand
import fpinscala.test.UnitSpec
import org.scalatest.BeforeAndAfter

/**
  * Created by Gnob on 2017. 3. 12..
  */
class RNGSpec extends UnitSpec with BeforeAndAfter {
  val rng: RNG = SimpleRNG(42)

  behavior of "SimpleRNG"

  "The SimpleRNG" should "be pure" in {
    val (n1, rng2) = rng.nextInt
    val (n11, _) = rng.nextInt
    val (n2, _) = rng2.nextInt
    val (n22, _) = rng2.nextInt

    assert(n1 == n11)
    assert(n2 == n22)
    assert(n1 != n2)
  }

  "wrongRandomPair()" should "generate same numbers" in {
    val (i1, i2) = RNG.wrongRandomPair(rng)
    assertResult(true) { i1 == i2 }
  }

  "randomPair()" should "perform well" in {
    val ((i1, i2), _) = RNG.randomPair(rng)
    assertResult(true) { i1 != i2 }
  }

  "nonNegativeInt()" should "perform well" in {
    val negativeInts = Stream.iterate(RNG.nonNegativeInt(rng))(x => RNG.nonNegativeInt(x._2))
    assertResult(true)(negativeInts.take(10).forall(x => x._1 >= 0))
  }

  "double()" should "perform well" in {
    val doubles = Stream.iterate(RNG.double(rng))(x => RNG.double(x._2))
    assertResult(true)(doubles.take(10).forall(x => x._1 >= 0 && x._1 < 1))
  }

  "ints()" should "perform well" in {
    val (ints, rng2) = RNG.ints(5)(rng)
    println(ints)
    println(rng2)
  }

  "Rand[]" should "make state transition" in {
  }
}
