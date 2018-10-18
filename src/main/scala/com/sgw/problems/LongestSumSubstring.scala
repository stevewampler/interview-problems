package com.sgw.problems

/**
  * Find length of longest substring of a given string of digits,
  * such that sum of digits in the first half and second half of
  * the substring is same.
  *
  * For example, Input: “142124”
  * Output: 6
  *
  * The whole string is answer, because, sum of first 3 digits = sum of last 3 digits (1+4+2 = 1+2+4).
  *
  * Input: “9430723”
  * Output: 4
  *
  * Longest substring with first and second half having equal sum is “4307”.
  *
  * Meenakshi. Dynamic Programming for Coding Interviews: A Bottom-Up approach to problem solving (p. 51). Notion Press. Kindle Edition.
  */
object LongestSumSubstring {
  private def sum(str: String): Int = str.map(_.toInt - '0').reduce[Int] { case (acc, chr) =>
    acc + chr
  }

  private def toNum(c: Char): Int = c - '0'

  private def goDeeper(str: String): Option[String] = {
    val maybeStr1 = go(str.substring(1))
    val maybeStr2 = go(str.substring(0, str.length-1))

    maybeStr1.flatMap { str1 =>
      maybeStr2.map { str2 =>
        if (str1.length >= str2.length) {
          Some(str1)
        } else {
          Some(str2)
        }
      }.getOrElse(Some(str1))
    } orElse maybeStr2
  }

  // brute force solution (O(n^3))
  private def go(str: String): Option[String] = {
    if (str.length < 2) return None

//    println(str)

    if (str.length % 2 == 0) {
      val str1 = str.substring(0, str.length / 2)
      val str2 = str.substring(str.length / 2)
      val sum1 = sum(str1)
      val sum2 = sum(str2)

//      println(s"str1=$str1, str2=$str2, sum1=$sum1, sum2=$sum2")
      if (sum1 == sum2) {
        Some(str)
      } else {
        goDeeper(str)
      }
    } else {
      goDeeper(str)
    }
  }

  def main(args: Array[String]): Unit = {
    val str = "3430721421243"

    val maybeResult = go(str)

    val msg = maybeResult.map { result =>
      s"string=$result, length=${result.length}"
    }.getOrElse("No substring found.")

    println(msg)

    assert(maybeResult.contains("3072142124"))

//    go2(str)
  }
}
