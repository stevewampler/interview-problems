package com.sgw.problems

import org.scalatest.{FlatSpec, Matchers}

class MyListSpec extends FlatSpec with Matchers {
  "An empty list" should "be empty" in {
    MyList().isEmpty should be (true)
  }

  "A list with one element" should "have a head but not a tail" in {
    MyList(1).isEmpty should be (false)
    MyList(1).head should be (1)
    MyList(1).tail.isEmpty should be (true)
  }

  "Indexing into a list with many items" should "return the ith item (if < list length)" in {
    MyList(1, MyList(2, MyList(3)))(1) should be (2)
  }

  "A list with many items" should "throw and exception i < 0" in {
    an [IndexOutOfBoundsException] should be thrownBy MyList(1, MyList(2, MyList(3)))(-1)
  }

  "A list with many items" should "throw and exception i >= the list length" in {
    an [IndexOutOfBoundsException] should be thrownBy MyList(1, MyList(2, MyList(3)))(3)
  }

  "A list" should "be able to be created with the cons operator (::)" in {
    1 :: 2 :: 3 :: MyNil should be (MyList(1, MyList(2, MyList(3))))
  }

  "A list" should "be able to be created using varargs" in {
    MyList(1, 2, 3) should be (MyList(1, MyList(2, MyList(3))))
  }

  "The last of a list" should "be the last element of the list" in {
    val list = MyList(5, 2, 1, 6, 8, 10, 50, 24, 20, 19, 3)
    list.last should be (3)
  }

  "The init of a list" should "be all but the last element" in {
    val list = MyList(5, 2, 1, 6, 8, 10, 50, 24, 20, 19, 3)
    list.init should be (MyList(5, 2, 1, 6, 8, 10, 50, 24, 20, 19))
  }

  "Taking n elements of a list" should "return a list of the first n elements" in {
    MyList().take(5) should be (MyList())
    MyList(5, 2, 1, 6, 8, 10, 50, 24, 20, 19, 3).take(5) should be (MyList(5, 2, 1, 6, 8))
  }

  "Dropping n elements of a list" should "return a list without the first n elements" in {
    MyList().drop(5) should be (MyList())
    MyList(5, 2, 1, 6, 8, 10, 50, 24, 20, 19, 3).drop(5) should be (MyList(10, 50, 24, 20, 19, 3))
  }

  "Splitting a list at the nth elements" should "return two lists" in {
    MyList(5, 2, 1, 6, 8, 10, 50, 24, 20, 19, 3).splitAt(5) should be (MyList(5, 2, 1, 6, 8), MyList(10, 50, 24, 20, 19, 3))
  }

  "The removeAt method" should "remove the ith element of a list" in {
    val list = MyList(5, 2, 1, 6, 8, 10, 50, 24, 20, 19, 3)

    list.removeAt(-5) should be (list)
    list.removeAt(0) should be (list.tail)
    list.removeAt(5) should be (MyList(5, 2, 1, 6, 8, 50, 24, 20, 19, 3))
    list.removeAt(10) should be (list.init)
    list.removeAt(11) should be (list)
  }

  "Concatenating two lists" should "concatenate the lists" in {
    val list1 = MyList(1, 2, 3)
    val list2 = MyList(4, 5, 6)

    list2.concat(list1) should be (MyList(1,2,3,4,5,6))

    list1 ::: list2 should be (MyList(1,2,3,4,5,6))
  }

  "Flattening a list" should "flatten the list" in {
    val list = MyList(1, MyList(2, 3), 4, MyList(5, 6, MyList(7, 8)))

    list.flatten should be (MyList(1, 2, 3, 4, 5, 6, 7, 8))
  }

  "The length of a list" should "be the length of the list" in {
    MyList().length should be (0)

    MyList(5, 2, 1, 6, 8, 10, 50, 24, 20, 19, 3).length should be (11)
  }

  "A list" should "be able to be sorted using an insertion sort" in {
    val list = MyList(5, 2, 1, 6, 8, 10, 50, 24, 20, 19, 3)

    list.insertionSort should be (MyList(1,2,3,5,6,8,10,19,20,24,50))
  }

  "A list" should "be able to be sorted using a merge sort" in {
    val list = MyList(5, 2, 1, 6, 8, 10, 50, 24, 20, 19, 3)

    list.mergeSort should be (MyList(1,2,3,5,6,8,10,19,20,24,50))
  }

  "Mapping over a list" should "return a transformed list" in {
    MyList(5, 2, 1, 6, 8).map(x => x + 1) should be (MyList(6, 3, 2, 7, 9))
  }

  "filter" should "return a filtered list" in {
    MyList(5, 2, 1, 6, 8).filter(x => x % 2 == 0) should be (MyList(2, 6, 8))
  }

  "filterNot" should "return a filtered list" in {
    MyList(5, 2, 1, 6, 8).filterNot(x => x % 2 == 0) should be (MyList(5, 1))
  }

  "partition" should "return a partitioned list" in {
    MyList(5, 2, 1, 6, 8).partition(x => x % 2 == 0) should be (MyList(2, 6, 8), MyList(5, 1))
  }

  "takeWhile" should "take the elements of the list while the predicate is true" in {
    MyList(4, 2, 3, 1, 2).takeWhile(x => x % 2 == 0) should be (MyList(4, 2))
  }

  "dropWhile" should "drop the elements of the list while the predicate is true" in {
    MyList(4, 2, 3, 1, 2).dropWhile(x => x % 2 == 0) should be (MyList(3, 1, 2))
  }

  "span" should "return the list's span" in {
    MyList(2, 4, 1, 6, 8).span(x => x % 2 == 0) should be (MyList(2, 4), MyList(1, 6, 8))
  }

  "pack" should "take the same elements of a list and returns a list of lists" in {
    MyList(1, 1, 1, 3, 3, 2, 5, 5).pack should be (MyList(MyList(1, 1, 1), MyList(3, 3), MyList(2), MyList(5, 5)))
  }

  "encode" should "generate the run-length encoding of a list" in {
    MyList(1, 1, 1, 3, 3, 2, 5, 5).encode should be (MyList((1, 3), (3, 2), (2, 1), (5, 2)))
  }

  "foldLeft" should "fold a function over a list from left to right" in {
    MyList(1, 2, 3).foldLeft(0) {
      case (acc, i) => acc + i
    } should be (6)
  }

  "foldRight" should "fold a function over a list from right to left" in {
    MyList(1, 2, 3).foldRight(0) {
      case (acc, i) => acc + i
    } should be (6)
  }
}
