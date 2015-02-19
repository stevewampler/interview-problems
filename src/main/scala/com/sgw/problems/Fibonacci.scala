package com.sgw.problems

import scala.annotation.tailrec

/**
 * Generates a fibonacci sequence: 0, 1, 1, 2, 3, 5, 8, 13, 21, ...
 */
object Fibonacci {
  @tailrec
  private def fibonacci(n: Int, seq: List[Int]): List[Int] = {
    if (n <= 2) return seq.reverse

    fibonacci(n - 1, (seq.head + seq.tail.head) :: seq)
  }

  def fibonacci(n: Int): List[Int] = fibonacci(n, List(1, 0))

  // using foldLeft
  def fibonacci2(n: Int): List[Int] =
    (0 until n - 2).foldLeft(List(1, 0))((z, _) => (z.head + z.tail.head) :: z).reverse

  def main (args: Array[String]) {
    println(fibonacci(11))
    println(fibonacci2(11))
  }
}
