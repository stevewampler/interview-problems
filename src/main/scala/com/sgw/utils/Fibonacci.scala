package com.sgw.utils

import scala.annotation.tailrec

/**
 * Generates a fibonacci sequence: 0, 1, 1, 2, 3, 5, 8, 13, 21, ...
 */
object Fibonacci {
  @tailrec
  private def fibonacci(n: Int, seq: List[Int]): List[Int] = {
    if (n <= 1) return seq.reverse

    fibonacci(n - 1, (seq.head + seq.tail.head) :: seq)
  }

  def fibonacci(n: Int): List[Int] = fibonacci(n, List(1, 0))

  // using foldLeft
  def fibonacci2(n: Int): List[Int] =
    (0 until n - 1).foldLeft(List(1, 0))((z, _) => (z.head + z.tail.head) :: z).reverse

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

  @tailrec
  def fib(i: Int, prev: Int = 1, prevprev: Int = 0): Int = {
    if (i < 0) {
      throw new RuntimeException("i must be >= 0")
    } else if (i == 0 || i == 1) {
      i
    } else if (i == 2)
      prev + prevprev
    else
      fib(i - 1, prev + prevprev, prev)
  }

  def fib2(i: Int): Int = {
    var n = i

    var p1 = 1
    var p2 = 0

    while(n > 1) {
      val tmp = p1 + p2

      p2 = p1

      p1 = tmp

      n = n - 1
    }

    p1
  }

  def main (args: Array[String]) {
    println(fibonacci(11))
    println(fibonacci2(11))
    println(fibonacci3(11).reverse)
    println(fib(11))
    println(fib2(11))
  }
}
