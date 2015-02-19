package com.sgw.problems

object Factorial {
  def factorial(n: Int): Int = {
    if (n == 0) return 1
    n * factorial(n - 1)
  }

  def main (args: Array[String]) {
    (0 to 10).map(n => (n, factorial(n))).map {
      case (n, fact) => s"$n! = $fact"
    }.foreach(println)
  }
}
