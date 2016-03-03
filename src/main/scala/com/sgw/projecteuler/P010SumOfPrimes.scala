package com.sgw.projecteuler

import com.sgw.problems.PrimeSieve

/**
 * From: https://projecteuler.net/problem=10
 *
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 *
 * Find the sum of all the primes below two million.
 *
 * Answer: 142913828922
 */
object P010SumOfPrimes extends App {
  val sum = sumPrimes(2000000)

  println(sum)

  assert(sum == 142913828922L)

  def sumPrimes(maxPrime: Int): Long = PrimeSieve.primes(maxPrime).map(_.toLong).sum
}
