package com.sgw.problems

/**
 * Given an input array of strings, where the first string is the number of subsequent strings in the array and the
 * subsequent strings contain two space-separated integers. Count the number integers in between the two specified
 * integers (inclusive on both ends) that are the square of some other integer.
 * 
 * For example, this inptut:
 * 
 * 2
 * 3 9
 * 17 24
 * 
 * Should produce the output/counts:
 * 
 * 2
 * 0
 * 
 * The first count (2) is because there are two integer whose square root is an integer between 3 and 9 (inclusive).
 * Specifically, those integers are 4 (2*2) and 9 (3*3). The 2nd count (0) is because there are no integers whose
 * square root is an integer between 17 and 24.
 *
 * Solution: The basic idea is to project each range into "square-root space" and then count the number of integers
 * between the projected range. We need to also "flatten" the square-root range to integers
 * (i.e. floor them), and then project the flattened square-roots back to the regular range space (by squaring the flattened
 * square roots). We can then check to see if the new range falls within the specified range. If so, we need to add
 * one to the count.
 */
object CountSquaredIntegers {
  /**
   * This is the brute force solution. Take too long to run for large ranges.
   */
  def bruteForceCountSquaredIntegers1(ranges: Array[(Int, Int)]): Array[Int] = {
    ranges.map {
      case (from, to) => (from to to).map(Math.sqrt(_)).count(v => v == v.toInt)
    }
  }

  def countSquaredIntegers(ranges: Array[(Int, Int)]): Array[Int] = ranges.
    map { // project the range values into the square root space
      case (from, to) => (from, to, Math.sqrt(from), Math.sqrt(to))
    }.map { // we only care about integer values, so flatten the square roots into integers
      case (from, to, fromSqrt, toSqrt) => (from, to, fromSqrt.floor.toInt, toSqrt.floor.toInt)
    }.map { // project the flattened sqrt values back to the non-square-root space (i.e. square them)
      case (from, to, fromSqrtFlattened, toSqrtFlattened) => (
        from, to,
        fromSqrtFlattened, toSqrtFlattened,
        fromSqrtFlattened * fromSqrtFlattened, toSqrtFlattened * toSqrtFlattened
      )
    }.map { // the number of integers between the flattened square root values is the count of squared integers
      case (from, to, fromSqrtFlattened, toSqrtFlattened, fromPrime, toPrime) => {
        // need to adjust the count by one if the reprojected range still falls within the original range
        toSqrtFlattened - fromSqrtFlattened + (if (fromPrime >= from && toPrime <= to) 1 else 0)
      }
    }

  def countSquaredIntegers(lines: Array[String]): Array[Int] = {
    val n = lines.headOption.map(_.toInt).getOrElse(0)
    
    val ranges = lines.drop(1).take(n).map(_.split(" ").map(_.toInt)).collect {
      case Array(a, b) => (a, b)
    }
    
    countSquaredIntegers(ranges)
  }
}
