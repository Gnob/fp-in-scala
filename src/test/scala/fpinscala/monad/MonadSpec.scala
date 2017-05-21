package fpinscala.monad

import fpinscala.state.State
import fpinscala.test.UnitSpec

/**
  * Created by Gnob on 2017. 5. 21..
  */
class MonadSpec extends UnitSpec {
  "Monad" should "perform replicateM()" in {
    val replica = Monad.optionMonad.replicateM(10, Some(30))
    val result = replica.get
    assertResult(10)(result.size)
    result.foreach(x => assertResult(30)(x))
  }

  "Monad" should "perform filter" in {
    val tc = List(1,2,3,4,5)
    val filtered = Monad.optionMonad.filterM(tc)(x => Option(x <= 3))
    val result = filtered.get
    assertResult(3)(result.size)
    result.foreach(x => assertResult(true)(x <= 3))
  }

  "Monad" should "perform for Id" in {
    val ab = Id("Hello, ") flatMap (a =>
      Id("monad!") flatMap (b =>
        Id(a + b)))

    val anotherAb = for {
      a <- Id("Hello, ")
      b <- Id("monad!")
    } yield a + b

    assertResult("Hello, monad!")(ab.value)
    assertResult("Hello, monad!")(anotherAb.value)
    assertResult(ab.value)(anotherAb.value)
  }

  "State Monad" should "perform well" in {
    val tcState = State((s:Int) => (s.toString, s + 1))
    val intStateMonad = Monad.stateMonad[Int]
    val replica = intStateMonad.replicateM(10, tcState)
    println(replica.run(4))

    val tc2State = State((s:Int) => (s.toDouble, s - 1))
    val map2 = intStateMonad.map2(tcState, tc2State)((a,b) => a + "/" + b.toString)
    println(map2.run(4))

    def zipWithIndex[A](as: List[A]): List[(Int,A)] =
      as.foldLeft(intStateMonad.unit(List[(Int,A)]()))((acc,a) => for {
        xs <- acc
        n <- State.get
        _ <- State.set(n + 1)
      } yield (n, a) :: xs).run(0)._1.reverse

    println(zipWithIndex(List("a","b","c","d")))
  }
}
