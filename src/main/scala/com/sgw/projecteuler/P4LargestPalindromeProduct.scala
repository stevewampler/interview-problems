package com.sgw.projecteuler

/**
 * From: https://projecteuler.net/problem=4
 *
 * A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
 *
 * Find the largest palindrome made from the product of two 3-digit numbers.
 */
object P4LargestPalindromeProduct {
  private def minNum(numDigits: Int): Int = Math.pow(10, numDigits - 1).toInt
  private def maxNum(numDigits: Int): Int = minNum(numDigits + 1) - 1

  private def reverse(num: Int): Int = num.toString.reverse.toInt

  private def isPalindrome(num: Int): Boolean = num == reverse(num)

  def main(args: Array[String]) {
    val numDigits = 3
    val min = minNum(numDigits)
    val max = maxNum(numDigits)

    val maxVals = (min to max).view.flatMap(i1 =>
      (i1 to max).view.map(i2 => (i1, i2, i1 * i2))
    ).filter {
      case (_, _, prod) => isPalindrome(prod)
    }
    .maxBy {
      case (_, _, prod) => prod
    }

    println(maxVals)
  }
}
