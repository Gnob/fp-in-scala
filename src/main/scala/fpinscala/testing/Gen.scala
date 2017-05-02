package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state.RNG.Rand
import fpinscala.state.{RNG, SimpleRNG, State}
import fpinscala.testing.OldPropV2.{FailedCase, SuccessCount}
import fpinscala.testing.Prop.TestCases
import fpinscala.testing.Prop.MaxSize

/**
  * Created by Gnob on 2017. 4. 12..
  */
class MockGen[+A] {
  def listOf[A](a: MockGen[A]): MockGen[List[A]] = ???
  def listOfN[A](n: Int, a: MockGen[A]): MockGen[List[A]] = ???
  def forAll[A](a: MockGen[A])(f: A => Boolean): OldPropV2 = ???
}

trait KidProp {
  def check(): Boolean
  def &&(p: KidProp): KidProp = () => {
    if (this.check())
      p.check()
    else
      false
  }
}

trait OldPropV1 {
  def check(): Boolean
  def &&(p: OldPropV1): OldPropV1 = new OldPropV1 {
    def check(): Boolean = OldPropV1.this.check && p.check
  }
}

object OldPropV2 {
  type FailedCase = String
  type SuccessCount = Int
}

trait OldPropV2 {
  def check(): Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: OldPropV2): OldPropV2 = ???
}


//object OldPropV3 {
//  type TestCases = Int
//  type Result = Either[(FailedCase, SuccessCount), SuccessCount]
//}
//
//case class OldPropV3(run: TestCases => Result)


object OldPropV4 {
  type Result = Option[(FailedCase, SuccessCount)]
}


sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified = true
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

case class PropV1(run: TestCases => Result)

case class PropV2(run: (TestCases,RNG) => Result)

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  def &&(p: Prop) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: Prop) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(max,n,rng)
      case x => x
    }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String) = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

//case class KidGen[A](sample: State[RNG,A]) {
//  def choose(start: Int, stopExclusive: Int): KidGen[Int] = {
//    def intRange(s: Int, e: Int): Rand[Int] =
//      RNG.flatMap(RNG.int)(x => if (s <= x && x < e) RNG.unit(x) else intRange(s, e))
//
//    sample.map(intRange(start, stopExclusive)(_))
//  }
//}

object Prop {
  type MaxSize = Int
  type TestCases = Int

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) => randomStream(as)(rng).zip(Stream.from(1)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i)}
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests: \n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def checkLazy(p: => Boolean): Prop = {
    lazy val result = p
    forAll(Gen.unit(()))(_ => result)
  }

  def checkPassed(p: => Boolean): Prop = Prop { (_,_,_) =>
    if (p) Passed else Falsified("()", 0)
  }

  def check(p: => Boolean): Prop = Prop { (_,_,_) =>
    if (p) Proved else Falsified("()", 0)
  }
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(_ % 2 == 0))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def both[A,B](a: Gen[A], b: Gen[B]): Gen[(A,B)] =
    Gen(a.sample.flatMap(x => b.sample.map(y => (x,y))))

  def chooseTuple(start: Int, stopExclusive: Int): Gen[(Int,Int)] =
    both(choose(start, stopExclusive), choose(start, stopExclusive))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1))
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(g.listOfN)
}

case class Gen[A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] = Gen(sample.map2(g.sample)(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)
  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap listOfN

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))
}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)
}
