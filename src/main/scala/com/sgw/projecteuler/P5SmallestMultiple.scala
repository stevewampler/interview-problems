package com.sgw.projecteuler

import com.sgw.problems.{PrimeSieve, PrimeSieveMutable}

/**
 * From: https://projecteuler.net/problem=5
 *
 * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
 *
 * What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
 */
object P5SmallestMultiple {
  def bruteForceSmallestMultiple(n: Int): Int =
    (1 to Integer.MAX_VALUE - 1).find(x => (1 to n).forall(f => x % f == 0)).getOrElse {
      throw new RuntimeException("This shouldn't happen!")
    }

  def fastSmallestMultiple(n: Int): Int = {
    val primes = PrimeSieve.primes(n)

    val limit = Math.sqrt(n)

    val logN = Math.log(n)

    primes.map(prime => if (prime <= limit) {
      Math.pow(prime, (logN / Math.log(prime)).floor)
    } else {
      prime
    }).product.toInt
  }

  def main(args: Array[String]) {
    val n = 20

//    val result1 = bruteForceSmallestMultiple(n)
//
//    assert(result1 == 232792560)

    val result2 = fastSmallestMultiple(n)

    println(result2)

    assert(result2 == 232792560)
  }
}
