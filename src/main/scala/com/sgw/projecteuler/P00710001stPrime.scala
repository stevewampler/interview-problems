package com.sgw.projecteuler

import com.sgw.problems.PrimeSieve

/**
 * From: https://projecteuler.net/problem=7
 *
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
 *
 * What is the 10001st prime number?
 */
object P00710001stPrime {
  def main(args: Array[String]) {
    val n = 10001

    val maybeNthPrime = PrimeSieve.prime(n)

    println(s"$n prime: $maybeNthPrime")

    assert(maybeNthPrime.exists(_ == 104743))
  }
}
