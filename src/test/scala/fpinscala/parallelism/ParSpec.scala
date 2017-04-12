package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.parallelism.Par._
import fpinscala.test.UnitSpec

/**
  * Created by Gnob on 2017. 3. 30..
  */
class ParSpec extends UnitSpec {
  val executorService = Executors.newFixedThreadPool(4)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  val equalInt: ((Par[Int], Par[Int]) => Boolean) = equal(executorService)

  behavior of "Par"

  "The map()" should "be" in {
    // 법칙을 정해보자
    map(unit(1))(_ + 1) == unit(2)

    // 여기서 동등하다는 의미는 어떤 것일까?
    assertResult(false)(map(unit(1))(_ + 1) == unit(2))
    // 위 결과는 false이다.

    // 우리가 기대하는 동등하다는 의미는 임의의 유효한 ExecutorService에 대해
    // 두 Par 객체의 Future 결과가 같다는걸 동등하다고 말하기로 하자

    // 그러기 위해선 법칙을 점검하기 위한 함수가 필요해진다
    assertResult(true)(equalInt(map(unit(1))(_ + 1), unit(2)))
  }

  it should "be also" in {
    val x = 1
    val f = (a: Int) => a + 1

    // 법칙을 일반화 할 수 있다
    map(unit(x))(f) == unit(f(x))

    assertResult(true)(equalInt(map(unit(x))(f), unit(f(x))))

    // 여기서 f를 항등 함수로 치환하고 관찰해보자
    map(unit(x))(f) == unit(f(x))
    map(unit(x))(identity) == unit(identity(x))
    map(unit(x))(identity) == unit(x)
    val y = unit(x)
    map(y)(identity) == y
  }

  it should "make free theorem" in {
    val f = (a: Int) => a + 1
    val g = (a: Int) => a * 4
    val x = 1
    val y = unit(x)
    map(y)(identity) == y
    map(map(y)(g))(f) == map(y)(f compose g)
  }
}
