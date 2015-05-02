package com.sgw.projecteuler

import com.sgw.problems.PrimeSieveMutable

import scala.annotation.tailrec

/**
 * From: https://projecteuler.net/problem=3
 *
 * The prime factors of 13195 are 5, 7, 13 and 29.
 *
 * What is the largest prime factor of the number 600851475143
 */
object P3LargestPrimeFactor {
  val primes = PrimeSieveMutable.findPrimes(10000)

  @tailrec
  def primeFactors(num: Long, primeIndex: Int = 0, factors: List[Long] = List()): List[Long] = {
    val currPrime = primes(primeIndex)

    if (currPrime >= num) return (num :: factors).reverse

    if (num % currPrime == 0) {
      val nextNum = num / currPrime

      primeFactors(nextNum, 0, currPrime :: factors)
    } else {
      primeFactors(num, primeIndex + 1, factors)
    }
  }

  def main(args: Array[String]) {
    // val num = 13195
    val num = 600851475143L

    val factors = primeFactors(num)

    println(factors.mkString(","))
  }
}
