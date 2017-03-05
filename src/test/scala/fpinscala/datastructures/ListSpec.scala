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

  it should "be existed" in {
    val list = Cons(1, Nil)
    assert(List.tail(list) == Nil)
    assert(List.setHead(list, 2) == Cons(2, Nil))

    assert(List.tail(Nil) == Nil)
    assert(List.setHead(Nil, 2) == Nil)

    val longList = List(1, 2, 3, 4)
    assert(List.tail(longList) == List(2, 3, 4))
    assert(List.setHead(longList, 2) == List(2, 2, 3, 4))
  }

  it should "be dropped" in {
    val longList = List(1, 2, 3, 4)
    assert(List.drop(longList, 1) == List(2, 3, 4))
    assert(List.drop(longList, 1) == List.tail(longList))
    assert(List.drop(longList, 2) == List(3, 4))
    assert(List.drop(longList, 3) == List(4))
    assert(List.drop(longList, 4) == Nil)
    assert(List.drop(longList, 5) == Nil)

    assert(List.dropWhile1(longList, (x: Int) => x < 1) == longList)
    assert(List.dropWhile1(longList, (x: Int) => x < 3) == List(3, 4))
    assert(List.dropWhile1(longList, (x: Int) => x < 5) == Nil)

    assert(List.dropWhile2(longList)(x => x < 4) == List(4))
  }

  it should "append and pop" in {
    val longList = List(1, 2, 3, 4)
    val shortList = List(5, 6)

    assert(List.init(longList) == List(1, 2, 3))
    assert(List.append(longList, shortList) == List(1, 2, 3, 4, 5 ,6))
  }

  it should "be folded right" in {
    val longList = List(1, 2, 3, 4)
    val doubleList = List(1.0, 2.0, 3.0, 4.0)
    assert(List.foldRight(longList, 0)(_ + _) == List.sum(longList))

    assert(List.sumByFoldRight(longList) == List.sum(longList))
    assert(List.productByFoldRight(doubleList) == List.product(doubleList))

    assert(List(1, 2, 3, 4) == List.foldRight(longList, Nil: List[Int])(Cons(_, _)))

    assert(List.lengthByFoldRight(doubleList) == 4)

    val loooongList = List.makeList2(1, 10000)
    assertThrows[StackOverflowError] { List.foldRight(loooongList, 0)(_ + _) }

    assert(List.foldLeft(longList, 0)(_ + _) == List.sum(longList))
  }

  it should "be folded left" in {
    val longList = List(1, 2, 3, 4)
    val doubleList = List(1.0, 2.0, 3.0, 4.0)
    assert(List.foldLeft(longList, 0)(_ + _) == List.sum(longList))
    assert(List.sumByFoldLeft(longList) == List.sum(longList))
    assert(List.productByFoldLeft(doubleList) == List.product(doubleList))

    assert(List(4, 3, 2, 1) == List.foldLeft(longList, Nil: List[Int])((x, y) => Cons(y, x)))

    assert(List.lengthByFoldLeft(doubleList) == 4)

    val loooongList = List.makeList2(1, 10000)
    assertResult(10000) { List.foldLeft(loooongList, 0)(_ + _) }
  }

  "FoldRight and FoldLeft" should "be equal" in {
    val testList = List("A", "B", "C", "D")

    assert(List.foldRight(testList, "")(_ + _) == List.foldLeft(testList, "")(_ + _))
    assert(List.foldRight(testList, "")(_ + _) != List.foldLeft(testList, "")((x, y) => y + x))
  }

  "List" should "append" in {
    val longList = List(1, 2, 3, 4)
    val shortList = List(5, 6)

    assert(List.appendByFoldRight(longList, shortList) == List(1, 2, 3, 4, 5 ,6))
  }

  "List" should "flatten" in {
    val listOfList: List[List[Int]] = List(List(1,2,3), List(4,5,6), List(7,8,9))

    assert(List.flatten(listOfList) == List(1,2,3,4,5,6,7,8,9))
  }

  "The integer list" should "increase" in {
    val longList = List(1, 2, 3, 4)

    assert(List.increase(longList) == List(2, 3, 4, 5))
    assert(List.map(longList)(x => x + 1) == List(2, 3, 4, 5))
  }

  "The double list" should "be converted String" in {
    val doubleList = List(1.0, 2.0, 3.0, 4.0)

    assert(List.doublesToString(doubleList) == List("1.0", "2.0", "3.0", "4.0"))
    assert(List.map(doubleList)(x => x.toString) == List("1.0", "2.0", "3.0", "4.0"))
  }

  "The integer list" should "be filtered" in {
    val longList = List(1, 2, 3, 4, 5, 6)

    assert(List.filter(longList)(x => x % 2 == 0) == List(2, 4, 6))
    assert(List.filterByFoldRight(longList)(x => x % 2 == 0) == List(2, 4, 6))
    assert(List.filterByFlatMap(longList)(x => x % 2 == 0) == List(2, 4, 6))
  }

  "The list" should "perform flatMap" in {
    val longList = List(1, 2, 3, 4)

    assert(List.flatMap(longList)(x => List(x, x, x)) == List(1,1,1,2,2,2,3,3,3,4,4,4))
  }

  "The list" should "perform addZip" in {
    val longList = List(1, 2, 3, 4)
    val longList2 = List(5, 6, 7, 8)

    assert(List.addZip(longList, longList2) == List(6, 8, 10, 12))
    assert(List.zipWith(longList, longList2)(_ + _) == List(6, 8, 10, 12))
  }

  "Subsequence" should "be found" in {
    val longList = List(1, 2, 3, 4)
    val subList1 = List(1, 2)
    val subList2 = List(2, 3)
    val subList3 = List(4)
    val subList4 = List(5, 6)
    val subList5 = List(1, 4)
    val subList6 = Nil
    val subList7 = List(1, 2, 3, 4, 5)
    val subList8 = List(2, 3, 4, 5)

    assert(List.hasSubsequence(longList, subList1))
    assert(List.hasSubsequence(longList, subList2))
    assert(List.hasSubsequence(longList, subList3))
    assert(!List.hasSubsequence(longList, subList4))
    assert(!List.hasSubsequence(longList, subList5))
    assert(!List.hasSubsequence(longList, subList6))
    assert(!List.hasSubsequence(longList, subList7))
    assert(!List.hasSubsequence(longList, subList8))

  }
}
