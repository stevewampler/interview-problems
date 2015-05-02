package com.sgw.problems

import java.io.InputStream

import scala.annotation.tailrec
import scala.io.Source

/**
 * From: http://www.yodlecareers.com/puzzles/triangle.html
 */
object Triangle {

  /**
   * Reads a triangle from a specified resource.
   */
  def fromResource(name: String): Array[Array[Int]] = fromInputStream(getClass.getResourceAsStream(name))

  /**
   * Reads a triangle from a specified input stream.
   */
  def fromInputStream(is: InputStream): Array[Array[Int]] = fromSource(Source.fromInputStream(is))

  /**
   * Reads a triangle from a specified file.
   */
  def fromFile(file: String): Array[Array[Int]] = fromSource(Source.fromFile(file))

  /**
   * Reads a triangle from the specified source. The triangle's data should be formatted as follows:
   *
   * {{{
   * 9235
   * 9096 637
   * 973 3269 7039
   * 3399 3350 4788 7546
   * 1739 8032 9427 976 2476
   * }}}
   *
   * where the triangle's values are separated by a single space and rows are separated by '\n' characters.
   * Each row should contain one more value than the previous row.
   *
   * @param source a Source containing the definition of a triangle
   * @return returns a triangle defined as an array of arrays of integers.
   */
  def fromSource(source: Source): Array[Array[Int]] =
    source.getLines().map(line => line.split(" ").map(_.toInt)).toArray

  /**
   * Finds the maximum sum of all the numbers in a triangle starting at the
   * top of the triangle and moving to adjacent numbers on the row below. For
   * example, the maximum total from top to bottom of the following triangle is 27
   * (5 + 9 + 6 + 7).
   *
   *       5
   *     9   6
   *   4   6   8
   * 0   7   1   5
   *
   * This implementation computes the sum bottom-up using reduceLeft, which is tail-recursive.
   *
   * @param triangle the triangle defined as an array of arrays of ints.
   */
  def findMaxSum(triangle: Array[Array[Int]]): Int = triangle
    .reverse // flip the triangle on its head
    .reduceLeft[Array[Int]] { // reduce
      case (row1, row2) =>             // map ...
        row1.init.zip(row1.tail).map { // pair up the larger row's values and ...
          case (v1, v2) => v1 max v2   // find the max value of the pair
        }.zip(row2).map {              // pair up the max values with the smaller row and ...
          case (x1, x2) => x1 + x2     // sum the pairs
        }
    }(0)

  //-----------------------------------------
  // Other implementations:

  /**
   * Recursively finds the maximum sum of all the numbers in a triangle starting at the
   * top of the triangle and moving to adjacent numbers on the row below. For
   * example, the maximum total from top to bottom of the following triangle is 27
   * (5 + 9 + 6 + 7).
   *
   *       5
   *     9   6
   *   4   6   8
   * 0   7   1   5
   *
   * This implementation works for small triangles, but isn't efficient cause
   * it calculates child sums more than once. It's also not tail recursive. It's good
   * for checking out other implementations though, so I'm keep'n it.
   *
   * @param triangle all or part of a triangle defined as an array of arrays of ints.
   */
  def findMaxSumTopDown(triangle: Array[Array[Int]]): Int = {
    def go(i: Int, triangle: Array[Array[Int]]): Int = {
      if (triangle.isEmpty) return 0

      triangle.head(i) + (go(i, triangle.tail) max go(i + 1, triangle.tail))
    }

    go(0, triangle)
  }

  /**
   * Recursively finds the maximum sum of all the numbers in a triangle starting at the
   * top of the triangle and moving to adjacent numbers on the row below. For
   * example, the maximum total from top to bottom of the following triangle is 27
   * (5 + 9 + 6 + 7).
   *
   *       5
   *     9   6
   *   4   6   8
   * 0   7   1   5
   *
   * This implementation computes the sum bottom-up, instead of top-down, to minimize the
   * number of duplicate calculations. It's also tail recursive.
   *
   * @param triangle the triangle defined as an array of arrays of ints.
   */
  def findMaxSumBottomUpRecursive(triangle: Array[Array[Int]]): Int = {
    @tailrec
    def go(acc: Array[Int], flippedTriangle: Array[Array[Int]]): Int = {
      val row = flippedTriangle.head.zip(acc).map { case (x1, x2) => x1 + x2}

      if (row.size == 1) return row(0)

      val newAcc = row.init.zip(row.tail).map {
        case (x1, x2) => x1 max x2
      }

      go(newAcc, flippedTriangle.tail)
    }

    go(Array.fill(triangle.size)(0), triangle.reverse)
  }
}
