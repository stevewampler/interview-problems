package com.sgw.problems

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

class MergeSortSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "A merge sort" should "sort an integer array" in {
    MergeSort.sort(Array[Int](5, 4, 6, 8, 10, 3, 7)) should be (Array[Int](3, 4, 5, 6, 7, 8, 10))
  }

  "A merge sort" should "return an empty array if given an empty array" in {
    MergeSort.sort(Array[Int]()) should be (Array[Int]())
  }

  "A merge sort" should "return an array of 1 if given an array of 1" in {
    MergeSort.sort(Array[Int](1)) should be (Array[Int](1))
  }
}
