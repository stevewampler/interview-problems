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

  // hmmmm
  def fibonacci3(n: Int, result: List[Int] = List[Int]()): List[Int] = {
    n match {
      case 0 => 0 :: result
      case 1 => 1 :: fibonacci3(n - 1, result)
      case _ => {
        val r = fibonacci3(n - 1, result)
        (r.head + r.tail.head) :: r
      }
    }
  }

  def main (args: Array[String]) {
    println(fibonacci(11))
    println(fibonacci2(11))
    println(fibonacci3(11).reverse)
  }
}
