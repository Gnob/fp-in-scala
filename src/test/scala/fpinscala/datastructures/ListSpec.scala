package fpinscala.datastructures

import fpinscala.test.UnitSpec

/**
  * Created by Gnob on 2017. 2. 28..
  */
class ListSpec extends UnitSpec {
  "List" should "be compiled" in {
    assertCompiles("val ex1: List[Double] = Nil")
    assertCompiles("val ex2: List[Int] = Cons(1, Nil)")
    assertCompiles("val ex3: List[String] = Cons(\"a\", Cons(\"b\", Nil))")
  }

  "List" should "be existed" in {
    val list = Cons(1, Nil)
    assert(List.tail(list) == Nil)
    assert(List.setHead(list, 2) == Cons(2, Nil))

    assert(List.tail(Nil) == Nil)
    assert(List.setHead(Nil, 2) == Nil)

    val longList = List(1, 2, 3, 4)
    assert(List.tail(longList) == List(2, 3, 4))
    assert(List.setHead(longList, 2) == List(2, 2, 3, 4))
  }

  "List" should "be dropped" in {
    val longList = List(1, 2, 3, 4)
    assert(List.drop(longList, 1) == List(2, 3, 4))
    assert(List.drop(longList, 1) == List.tail(longList))
    assert(List.drop(longList, 2) == List(3, 4))
    assert(List.drop(longList, 3) == List(4))
    assert(List.drop(longList, 4) == Nil)
    assert(List.drop(longList, 5) == Nil)

    assert(List.dropWhile(longList, (x: Int) => x < 1) == longList)
    assert(List.dropWhile(longList, (x: Int) => x < 3) == List(3, 4))
    assert(List.dropWhile(longList, (x: Int) => x < 5) == Nil)
  }
}
