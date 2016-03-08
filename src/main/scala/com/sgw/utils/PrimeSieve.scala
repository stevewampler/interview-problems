package com.sgw.utils

import java.lang.Math._

/**
 * Uses a Sieve of Eratosthenes to find prime numbers.
 *
 * See: http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
 *
 * The bounds of the nth prime number is:
 *
 * n ln n + n(ln ln n − 1) < pn < n ln n + n ln ln n for n ≥ 6
 *
 * See: http://math.stackexchange.com/questions/1257/is-there-a-known-mathematical-equation-to-find-the-nth-prime
 */
object PrimeSieve {
  private def createSieve(maxPrime: Int): Array[Boolean] = {
    val sieve = new Array[Boolean](maxPrime)
    sieve(1) = true // 1 is not prime, so mark it
    (0 until maxPrime by 2).foreach(i => sieve(i) = true) // mark all even numbers
    sieve
  }
  
  /**
   * Returns a list of all of the prime numbers less than the number n.
   * 
   * @param maxPrime the upper limit on the list of prime numbers
   */
  def primesList(maxPrime: Int): List[Int] = {
    val sieve = createSieve(maxPrime)

    (3 to maxPrime by 2).foldLeft(List[Int](2)) {
      case (primeNumbers, num) =>
        findNextPrime(primeNumbers.head, sieve).map(primeNumber => {
          markFactors(primeNumber, sieve)
          primeNumber :: primeNumbers
        }).getOrElse(primeNumbers)
    }
  }.reverse

  /**
   * Returns an iterator of all of the prime numbers less than the number maxPrime from largest to smallest.
   *
   * @param maxPrime the maximum prime number
   */
  def primes(maxPrime: Int): Iterator[Int] = {
    val sieve = createSieve(maxPrime)

    var lastPrime = 2 // start with two (is prime)

    // iterate over the odd numbers
    val iterator = (3 to maxPrime by 2).toIterator.map(num =>
      findNextPrime(lastPrime, sieve).map(primeNumber => {
        markFactors(primeNumber, sieve)
        lastPrime = primeNumber
        primeNumber
      })
    ).takeWhile(maybePrime => maybePrime.isDefined).map(_.get)

    // wrap the above iterator in one that will also return 2
    new Iterator[Int] {
      private var isFirst = true

      def hasNext: Boolean =isFirst || iterator.hasNext

      def next(): Int = if (isFirst) {
        isFirst = false
        2
      } else {
        iterator.next()
      }
    }
  }

  /**
   * Returns the bounds of the nth prime number.
   */
  def nthPrimeBounds(n: Int): (Int, Int) = {
    val logN = log(n)
    val logLogN = log(logN)
    val nLogN = n * logN
    val nLogLogN = n * logLogN

    ((nLogN + nLogLogN - n).floor.toInt, (nLogN + nLogLogN).ceil.toInt)
  }

  /**
   * Returns the nth prime number.
   */
  def prime(n: Int): Option[Int] = {
    if (n <= 1) return None

    if (n < 6) {
      primesList(20).drop(n-1).headOption
    } else {
      val (_, upperBounds) = nthPrimeBounds(n)
      primesList(upperBounds).drop(n-1).headOption
    }
  }

  private def findNextPrime(prime: Int, sieve: Array[Boolean]): Option[Int] = {
    val iterator = (prime until sieve.length).toIterator.map(i => (i, sieve(i))).dropWhile {
      case (_, isMarked) => isMarked // drop until we find a prime
    }.map {
      case (i, _) => i // return the prime number
    }

    if (iterator.hasNext) Some(iterator.next()) else None
  }

  private def markFactors(prime: Int, sieve: Array[Boolean]): Unit =
    (prime until sieve.length by prime * 2).foreach(i => sieve(i) = true) // factors of the prime are not prime
}
