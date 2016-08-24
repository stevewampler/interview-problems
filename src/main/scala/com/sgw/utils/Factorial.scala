package com.sgw.utils

import scala.annotation.tailrec

/**
 * Print the factorials for the numbers 0 through 10.
 */
object Factorial {
  def factorial(n: Int): Int = {
    if (n == 0) return 1
    n * factorial(n - 1)
  }


  def factorial2(n: Int): Int = {
    @tailrec
    def go(n: Int, result: Int = 1): Int = if (n == 0) result else go(n - 1, n * result)

    go(n)
  }

  def main (args: Array[String]) {
    (0 to 10).map(n => (n, factorial(n), factorial2(n))).map {
      case (n, fact, fact2) => s"$n! = $fact = $fact2"
    }.foreach(println)
  }
}
