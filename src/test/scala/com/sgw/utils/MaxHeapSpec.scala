package com.sgw.utils

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

class MaxHeapSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "A MaxHeap" should "sort the largest items first" in {
    val arr = Array(5, 7, 2, 10, 2, 7, 9, 4, 3)

    MaxHeap(arr).toList should be(List(10, 9, 7, 7, 5, 4, 3, 2, 2))
  }

  "A MaxHeap" should "return the largest items first" in {
    val arr = Array(5, 7, 2, 10, 2, 7, 9, 4, 3)

    val heap = MaxHeap(arr)
    heap.headOption should be(Some(10))
    heap.headOption should be(Some(10))

    heap.drop(1)
    heap.headOption should be(Some(9))

    heap.drop(2)
    heap.headOption should be(Some(7))

    heap.takeAndDropWhile(_ >= 5) should be(List(7, 5))

    heap.filter(_ != 3) should be(List(4, 2, 2))
  }
}
