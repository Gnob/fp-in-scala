package fpinscala.laziness

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
    val sx = Stream.cons({ println("expensive in cons"); 1}, tl)
    x.headOption
    x.headOption
    sx.headOption
    sx.headOption
  }

  "toList()" should "not be lazy" in {
    val xs = Stream({ println("one"); 1 }, { println("two"); 2 }, { println("three"); 3 })
    assert(xs.toList.isInstanceOf[List[_]])
  }

  "take() and drop()" should "perform well" in {
    val xs = Stream(1,2,3,4,5,6)
    assert(xs.take(3).toList == Stream(1,2,3).toList)
    assert(xs.drop(3).toList == Stream(4,5,6).toList)
    assert(xs.takeWhile(x => x <= 3).toList == Stream(1,2,3).toList)
  }
}
