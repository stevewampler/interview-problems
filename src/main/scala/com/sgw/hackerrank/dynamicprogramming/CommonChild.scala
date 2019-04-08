package com.sgw.hackerrank.dynamicprogramming

import scala.annotation.tailrec

/**
  * A string is said to be a child of a another string if it can be formed by deleting 0 or more characters from the
  * other string. Given two strings of equal length, what's the longest string that can be constructed such that it
  * is a child of both?
  *
  * For example, ABCD and ABDC have two children with maximum length 3, ABC and ABD.
  * They can be formed by eliminating either the D or C from both strings. Note that we will not consider ABCD
  * as a common child because we can't rearrange characters and ABCD != ABDC.
  *
  * From: https://www.hackerrank.com/challenges/common-child/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=strings
  */
object CommonChild {
  /**
    * This is the recursive brute force solution. It actually returns the longest child string of s1 and s2.
    */
  def commonChildBruteForce(s1: String, s2: String): String = {
    if (s1.isEmpty || s2.isEmpty) return ""

    if (s1.charAt(0) == s2.charAt(0)) {
      s1.charAt(0) + commonChildBruteForce(s1.substring(1), s2.substring(1))
    } else {
      val r1 = commonChildBruteForce(s1, s2.substring(1))
      val r2 = commonChildBruteForce(s1.substring(1), s2)

      if (r1.length > r2.length) {
        r1
      } else {
        r2
      }
    }
  }

  def commonChild2(
    s1: String,
    s2: String
  ): String = {
    @tailrec
    def go(
      row: Int = 1,
      col: Int = 1,
      arr: Array[Array[String]] = Array.fill[String](s1.length + 1, s2.length + 1)(elem = "")
    ): (Array[Array[String]], String) = {
      val c1 = s1.charAt(row - 1)
      val c2 = s2.charAt(col - 1)

      val s = if (c1 == c2) {
        arr(row - 1)(col - 1) + c1.toString
      } else {
        val r1 = arr(row - 1)(col)
        val r2 = arr(row)(col - 1)

        if (r1.length > r2.length) {
          r1
        } else {
          r2
        }
      }

      arr(row)(col) = s

      if (col < s2.length) {
        go(row, col + 1, arr)
      } else if (row < s1.length) {
        go(row + 1, 1, arr)
      } else {
        arr -> arr(row)(col)
      }
    }

    val (arr, result) = go()

//    println(s1)
//    println(s2)
//    arr.foreach { row =>
//      println(row.mkString(","))
//    }

    result
  }

  /**
    * Returns the max length of a string that's a child of both s1 and s2.
    *
    * This solution uses dynamic programming to memoize the solution to smaller problems.
    * For example, if the two strings are "ABC" and "BCA", the solution is "BC". Here's how the building of the
    * array progresses, where the answer ends up in the lower right cell of the array.
    *
    * The n+1 X n+1 array's first row and column are always zeros. They represent the solution to the sub-problems
    * of an empty string vs. one of the specified strings, which always yield children strings of length 0.
    *
    *     B C A
    *   0 0 0 0
    * A 0
    * B 0
    * C 0
    *
    * Process "ABC"'s A against "BCA". When "A" match the next character in "BCA", you take the solution at arr(row - 1)(col - 1)
    * and add one to it and save it at arr(row)(col).
    * If "A" does not match the next character in "BCA", take the max of arr(row-1)(col) and arr(row)(col - 1) and assign it
    * to arr(row)(col).
    *
    *     B C A
    *   0 0 0 0
    * A 0 0 0 1
    * B 0
    * C 0
    *
    * Moving on to the character "B" in "ABC" ...
    *
    *     B C A
    *   0 0 0 0
    * A 0 0 0 1
    * B 0 1 1 1
    * C 0
    *
    * And now the "C" in "ABC" ...
    *
    *     B C A
    *   0 0 0 0
    * A 0 0 0 1
    * B 0 1 1 1
    * C 0 1 2 2
    *
    * So the answer is arr(n)(n) => 2
    *
    * @param s1
    * @param s2
    */
  def commonChild(s1: String, s2: String): Int = {
    assert(s1.length == s2.length)

    val n = s1.length

    val arr = Array.ofDim[Int](n + 1, n + 1)

    (1 to n).foreach { row =>
      (1 to n).foreach { col =>
        val c1 = s1.charAt(row - 1)
        val c2 = s2.charAt(col - 1)

        arr(row)(col) = if (c1 == c2) {
          arr(row - 1)(col - 1) + 1
        } else {
          arr(row - 1)(col).max(arr(row)(col - 1))
        }
      }
    }

    println(s1)
    println(s2)
    arr.foreach { row =>
      println(row.mkString(","))
    }

    arr(n)(n)
  }

  def main(args: Array[String]): Unit = {
    assert(commonChild(s1 = "ABCD", s2 = "ABDC") == 3)
    assert(commonChild(s1 = "HARRY", s2 = "SALLY") == 2)
    assert(commonChild(s1 = "AA", s2 = "BB") == 0)
    assert(commonChild(s1 = "SHINCHAN", s2 = "NOHARAAA") == 3)
    assert(commonChild(s1 = "ABCDEF", s2 = "FBDAMN") == 2)
    assert(commonChild(s1 = "OUDFRMYMAW", s2 = "AWHYFCCMQX") == 2)
    assert(commonChild(s1 = "AAA", s2 = "AAA") == 3)
    assert(commonChild(s1 = "ABAABAA", s2 = "ZBAABAZ") == 5)
    assert(commonChild(s1 = "YAHOO", s2 = "AOOYH") == 3)

    assert(commonChild2(s1 = "ABCD", s2 = "ABDC") == "ABD")
    assert(commonChild2(s1 = "HARRY", s2 = "SALLY") == "AY")
    assert(commonChild2(s1 = "AA", s2 = "BB") == "")
    assert(commonChild2(s1 = "SHINCHAN", s2 = "NOHARAAA") == "NHA")
    assert(commonChild2(s1 = "ABCDEF", s2 = "FBDAMN") == "BD")
    assert(commonChild2(s1 = "OUDFRMYMAW", s2 = "AWHYFCCMQX") == "AW")
    assert(commonChild2(s1 = "AAA", s2 = "AAA") == "AAA")
    assert(commonChild2(s1 = "ABAABAA", s2 = "ZBAABAZ") == "BAABA")
    assert(commonChild2(s1 = "YAHOO", s2 = "AOOYH") == "AOO")
  }
}
