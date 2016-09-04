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
}
