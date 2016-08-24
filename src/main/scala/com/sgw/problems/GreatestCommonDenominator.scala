package com.sgw.problems

/**
  * Calculates the GCD of two numbers using the Euclidean algorithm (https://en.wikipedia.org/wiki/Euclidean_algorithm)
  */
object GreatestCommonDenominator extends App {
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  println(gcd(14, 21))
}
