package com.sgw.problems

import org.scalatest.{Matchers, FlatSpec}

class TriangleSpec extends FlatSpec with Matchers {
  private val triangle = Array(
    Array(5),
    Array(9, 6),
    Array(4, 6, 8),
    Array(0, 7, 1, 5)
  )

  "The Triangle.fromResource method" should "return a triangle read from a resource" in {
    val triangle = Triangle.fromResource("/triangle.txt")

    triangle.size should be (100)
    triangle(0) should be (Array(9235))
    triangle(10) should be (Array(8117, 4602, 7324, 7545, 4014, 6970, 4342, 7682, 150, 3856, 8177))
  }

  "The Triangle.findMaxSumTopDown" should "return the maximum sum of a triangle" in {
    Triangle.findMaxSumTopDown(triangle) should be (27)
  }

  "The Triangle.findMaxSumBottomUpRecursive" should "return the maximum sum of a triangle" in {
    Triangle.findMaxSumBottomUpRecursive(triangle) should be (27)
  }

  "The Triangle.findMaxSum method" should "return the maximum sum of a triangle" in {
    Triangle.findMaxSum(triangle) should be (27)
  }

  "The Triangle.findMaxSumTopDown method" should "return the maximum sum of a large triangle read from a file" in {
    Triangle.findMaxSumBottomUpRecursive(Triangle.fromResource("/triangle.txt")) should be (732506)
  }

  "The Triangle.findMaxSum method" should "return the maximum sum of a large triangle read from a file" in {
    Triangle.findMaxSum(Triangle.fromResource("/triangle.txt")) should be (732506)
  }
}
