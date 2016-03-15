package com.sgw.utils

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class HeapsortSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "The Heapsort algorithm" should "sort an array of integers" in {
    val arr = Array(5, 7, 2, 10, 2, 7, 9, 4, 3)

    Heapsort.heapsort(arr) should be(Array(2, 2, 3, 4, 5, 7, 7, 9, 10))
  }
}
