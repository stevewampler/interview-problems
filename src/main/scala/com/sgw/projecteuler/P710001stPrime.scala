package com.sgw.projecteuler

import com.sgw.problems.PrimeSieveMutable

/**
 * author: steve
 */
object P710001stPrime {
  def main(args: Array[String]) {
    val n = 10001
    val n2 = (n * Math.log(n)).toInt
    val n3 = ((n + 1) * Math.log(n + 1)).toInt

    println(1/Math.log(n))

    println(n2)
    println(n3)

    // val primes = PrimeSieveMutable.findPrimes(n2)

    // println(primes.size)
    // println(primes.last)
  }
}
