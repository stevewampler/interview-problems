package com.sgw.utils

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

class TrianglesSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "An iterator over the first 10 triangle numbers" should "return the correct list of triangle numbers" in {
    Triangles.generate.take(10).toList should be(List(1, 3, 6, 10, 15, 21, 28, 36, 45, 55))
  }
}
