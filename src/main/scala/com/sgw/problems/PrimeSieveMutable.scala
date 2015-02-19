package com.sgw.problems

import scala.collection.mutable.ArrayBuffer

/**
 * Finds all of the primes between 2 and n.
 *
 * See: http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
 */
object PrimeSieveMutable {
  def findPrimes(n: Int): List[Int] = findPrimesImpl(new Array[Boolean](n), Nil)

  private def findPrimesImpl(marked: Array[Boolean], primes: List[Int]): List[Int] =
    findNextPrime(marked).map(prime => {
      markFactors(prime, marked)
      findPrimesImpl(marked, prime :: primes)
    }).getOrElse(primes.reverse)

  private def findNextPrime(marked: Array[Boolean]): Option[Int] = {
    var i = 2

    while(i < marked.size && marked(i)) {
      i = i + 1
    }

    if (i < marked.size) Some(i) else None
  }

  private def markFactors(prime: Int, marked: Array[Boolean]): Unit =
    (prime until marked.size by prime).foreach(i => marked(i) = true)
}
