package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state.RNG.Rand
import fpinscala.state.{RNG, State}
import fpinscala.testing.OldPropV2.{FailedCase, SuccessCount}
import fpinscala.testing.OldPropV3.TestCases

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


object OldPropV3 {
  type TestCases = Int
  type Result = Either[(FailedCase, SuccessCount), SuccessCount]
}

case class OldPropV3(run: TestCases => Result)


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

case class PropV1(run: TestCases => Result)

case class Prop(run: (TestCases,RNG) => Result)

trait Prop {
  def &&(p: Prop): Prop = ???
  def ||(p: Prop): Prop = ???
}


case class KidGen[A](sample: State[RNG,A]) {
  def choose(start: Int, stopExclusive: Int): KidGen[Int] = {
    def intRange(s: Int, e: Int): Rand[Int] =
      RNG.flatMap(RNG.int)(x => if (s <= x && x < e) RNG.unit(x) else intRange(s, e))

    sample.map(intRange(start, stopExclusive)(_))
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

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = ???

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(1)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i)}
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(g.listOfN)
}

case class Gen[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)
  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap listOfN

  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](forSize: Int => Gen[A])

object SGen {
  type MaxSize = Int
  case class Prop(run: (MaxSize,TestCases,RNG) => Result)

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

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
}
