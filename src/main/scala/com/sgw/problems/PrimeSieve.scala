package com.sgw.problems

/**
 * Finds all of the primes between 2 and n.
 *
 * See: http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
 */
object PrimeSieve {
  def findPrimes(n: Int): List[Int] = {
    findPrimesImpl((2 to n).map(num => (false, num)), Nil)
  }

  private def findPrimesImpl(marked: Seq[(Boolean, Int)], primes: List[Int]): List[Int] =
    findNextPrime(marked).map(prime => findPrimesImpl(markFactors(prime, marked), prime :: primes)).getOrElse(primes.reverse)

  private def findNextPrime(marked: Seq[(Boolean, Int)]): Option[Int] = marked.dropWhile {
    case (mark, _) => mark
  }.headOption.map(head => head._2)

  private def markFactors(prime: Int, marked: Seq[(Boolean, Int)]): Seq[(Boolean, Int)] = marked.map {
    case (mark, num) => (mark || num % prime == 0, num)
  }
}
