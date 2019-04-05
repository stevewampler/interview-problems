package com.sgw.problems

object NextPrime {
  // assuming i is odd
  private def isPrime(n: Int): Boolean = {
    var i = 3

    while (n % i != 0 && i < Math.sqrt(n)) {
      i = i + 2
    }

    n % i != 0
  }

  def nextPrime(n: Int): Int = {
    var i = if (n % 2 == 0) {
      n + 1
    } else {
      n + 2
    }

    while(!isPrime(i)) {
      i = i + 2 // only need to check the odd numbers
    }

    i
  }

  def main(args: Array[String]): Unit = {
    assert(nextPrime(6) == 7)
    assert(nextPrime(7) == 11)
    assert(nextPrime(11) == 13)
    assert(nextPrime(13) == 17)
  }
}
