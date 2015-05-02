package com.sgw.projecteuler

/**
 * From: https://projecteuler.net/problem=1
 *
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
 *
 * Find the sum of all the multiples of 3 or 5 below 1000.
 */
object P1MultiplesOf3And5 {
  def main (args: Array[String]) {
    val result1 = (1 until 1000).filter(i => i % 3 == 0 || i % 5 == 0).sum

    println(result1)
    assert(result1 == 233168)

    // or ...
    val by3 = (0 until 1000 by 3).sum
    val by5 = (0 until 1000 by 5).filter(_ % 3 != 0).sum

    val result2 = by3 + by5

    println(result2)
    assert(result2 == result1)
  }
}
