package com.sgw.problems

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
  /**
   * Returns a list of all of the prime numbers less than the number n.
   * 
   * @param n the upper limit on the list of prime numbers
   *          
   * @return
   */
  def primes(n: Int): List[Int] = {
    val sieve = (0 until n).map(num => (num % 2 == 0, num)).toArray

    (3 to n by 2).foldLeft(List[Int](2)) {
      case (primeNumbers, num) =>
        findNextPrime(primeNumbers.head, sieve).map(primeNumber => {
          markFactors(primeNumber, sieve)
          primeNumber :: primeNumbers
        }).getOrElse(primeNumbers)
    }
  }.reverse

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
      primes(20).drop(n-1).headOption
    } else {
      val (_, upperBounds) = nthPrimeBounds(n)
      primes(upperBounds).drop(n-1).headOption
    }
  }

  private def findNextPrime(prime: Int, sieve: Array[(Boolean, Int)]): Option[Int] =
    (prime until sieve.size).dropWhile(i => sieve(i)._1).headOption.map(i => sieve(i)._2)

  private def markFactors(prime: Int, sieve: Array[(Boolean, Int)]): Unit =
    (prime until sieve.size by prime).foreach(i => sieve(i) = (true, sieve(i)._2))
}
