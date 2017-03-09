package fpinscala.laziness

import fpinscala.laziness.Stream.{cons}
import fpinscala.test.UnitSpec

/**
  * StreamSpec
  *
  * @author Gnob
  * @since 2017. 03. 06.
  */
class StreamSpec extends UnitSpec{
  behavior of "Using Cons directly"

  "Cons" should "not be laziness" in {
    val tl = Stream(1, 2, 3)
    val x = Cons(() => { println("expensive in Cons"); 1}, () => tl)
    val sx = cons({ println("expensive in cons"); 1}, tl)
    val n: Stream[Int] = Stream.empty

    x.headOption
    x.headOption
    sx.headOption
    sx.headOption
    assert(sx.headOption_1.contains(1))
    assert(n.headOption_1.isEmpty)
  }

  "toList()" should "not be lazy" in {
    val xs = Stream({ println("one"); 1 }, { println("two"); 2 }, { println("three"); 3 })

    assert(xs.toList.isInstanceOf[List[_]])
  }

  "take() and drop()" should "perform well" in {
    val xs = Stream(1,2,3,4,5,6)

    assert(xs.take(3).toList == Stream(1,2,3).toList)
    assert(xs.take_1(3).toList == Stream(1,2,3).toList)
    assert(xs.drop(3).toList == Stream(4,5,6).toList)
    assert(xs.takeWhile(x => x <= 3).toList == Stream(1,2,3).toList)
    assert(xs.takeWhile_1(x => x <= 3).toList == Stream(1,2,3).toList)
    assert(xs.takeWhile_2(x => x <= 3).toList == Stream(1,2,3).toList)
    assert(xs.notStackSafeExists(_ == 3))
    assert(xs.exists(_ == 3))
    assert(xs.forAll(_ < 7))
    assert(!xs.forAll(_ < 2))
    assert(xs.map(_.toString).toList == Stream("1","2","3","4","5","6").toList)
    assert(xs.map_1(_.toString).toList == Stream("1","2","3","4","5","6").toList)
    assert(xs.append(Stream(7, 8, 9)).toList == List(1,2,3,4,5,6,7,8,9))
    assert(xs.flatMap(x => Stream(x, x, x)).toList == List(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6))
  }

  "foldRight" should "perform well" in {
    val xs = Stream(1,2,3,4,5,6)

    assert(xs.nonLazyFoldRight(Empty: Stream[Int])((x, y) => Cons(() => x, () => y)).toList == xs.toList)
    assert(xs.foldRight(Empty: Stream[Int])((x, y) => Cons(() => x, () => y)).toList == xs.toList)
  }

  "Stream tracing" should "perform as interleaving loop" in {
    println(Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList)
    println(cons(11, Stream(2,3,4).map(_ + 10)).filter(_ % 2 == 0).toList) // apply map() for first element
    println(Stream(2,3,4).map(_ + 10).filter(_ % 2 == 0).toList) // apply filter() for first element -> as interleaving!
    println(cons(12, Stream(3,4).map(_ + 10)).filter(_ % 2 == 0).toList)
    println(12 :: Stream(3,4).map(_ + 10).filter(_ % 2 == 0).toList)
    println(12 :: cons(13, Stream(4).map(_ + 10)).filter(_ % 2 == 0).toList)
    println(12 :: Stream(4).map(_ + 10).filter(_ % 2 == 0).toList)
    println(12 :: cons(14, Stream[Int]().map(_ + 10)).filter(_ % 2 == 0).toList)
    println(12 :: 14 :: Stream[Int]().map(_ + 10).filter(_ % 2 == 0).toList)
    println(12 :: 14 :: List())
  }

  "The ones()" should "be infinite Stream" in {
    lazy val ones: Stream[Int] = Stream.cons(1, ones)

    assert(ones.take(5).toList == List(1,1,1,1,1))
    assert(ones.exists(_ % 2 != 0))
    assert(ones.map(_ + 1).exists(_ % 2 == 0))
    ones.takeWhile_1(_ == 1)
    assertThrows[StackOverflowError] { ones.takeWhile_1(_ == 1).toList }
    assert(!ones.forAll(_ != 1))
  }

  "The constant()" should "be infinite Stream" in {
    lazy val ones: Stream[Int] = Stream.constant_1(1)

    assert(ones.take(5).toList == List(1,1,1,1,1))
    assert(ones.exists(_ % 2 != 0))
    assert(ones.map(_ + 1).exists(_ % 2 == 0))
    ones.takeWhile_1(_ == 1)
    assertThrows[StackOverflowError] { ones.takeWhile_1(_ == 1).toList }
    assert(!ones.forAll(_ != 1))
  }

  "The from()" should "be infinite Stream" in {
    lazy val intSeq: Stream[Int] = Stream.from_1(1)

    assert(intSeq.take(5).toList == List(1,2,3,4,5))
    assert(intSeq.exists(_ % 2 == 0))
    assert(intSeq.map(_ + 1).exists(_ % 2 != 0))
    intSeq.takeWhile_1(_ < 1000000)
    assertThrows[StackOverflowError] { intSeq.takeWhile_1(_ >= 0).toList }
    assert(!intSeq.forAll(_ <= 5))
  }

  "The fibs()" should "be infinite Stream" in {
    lazy val fiboSeq: Stream[Int] = Stream.fibs_1()

    assert(fiboSeq.take(12).toList == List(0,1,1,2,3,5,8,13,21,34,55,89))
  }

  "The zipWith()" should "make two stream zippy" in {
    val xs: Stream[Int] = Stream(1,2,3,4,5,6)
    val ys: Stream[String] = Stream("boy", "girl", "man", "woman", "father", "mother")
    val zs: Stream[String] = Stream("boy", "girl", "man", "woman", "father", "mother", "grandpa", "grandma")

    assert(xs.zipWith(ys)((x,y) => x.toString + y).toList
      == Stream("1boy", "2girl", "3man", "4woman", "5father", "6mother").toList)

    println(xs.zipAll(zs).toList)
  }

  "The tails()" should "be Stream of Stream" in {
    val xs: Stream[Int] = Stream(1,2,3,4)

    assert(xs.tails.toList.map(_.toList) == List(List(1,2,3,4),List(2,3,4),List(3,4),List(4)))
  }
}
