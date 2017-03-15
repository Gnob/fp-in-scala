package fpinscala.state

import fpinscala.test.UnitSpec

/**
  * Created by Gnob on 2017. 3. 16..
  */
class StateSpec extends UnitSpec {
  type Rand[A] = State[RNG,A]
  val rng: RNG = SimpleRNG(42)

  behavior of "State"

  "State" should "be pure" in {
    println(State.sequence(List(State.unit[RNG, Int](1), State.unit[RNG, Int](2), State.unit[RNG, Int](3), State.unit[RNG, Int](4))).run(rng))

    println(State(RNG.int).map(x => x + 1).run(rng))
    println(State(RNG.int).map2(State(RNG.double))((x, y) => x + y).run(rng))
    println(State(RNG.int).flatMap(x => State(s => (x + 1, s))).run(rng))
  }
}
