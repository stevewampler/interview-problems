package com.sgw.projecteuler

import scala.annotation.tailrec

/**
  * From: https://projecteuler.net/problem=13
  *
  * The following iterative sequence is defined for the set of positive integers:
  *
  *   n → n/2 (n is even)
  *   n → 3n + 1 (n is odd)
  *
  * Using the rule above and starting with 13, we generate the following sequence:
  *
  *   13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
  *
  * It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
  * Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
  *
  * Which starting number, under one million, produces the longest chain?
  */
object P014LongestCollatzSequence {
//  def next(n: Long): Long = if (n % 2 == 0) nextIfEven(n) else nextIfOdd(n)
//  def nextIfEven(n: Long): Long = n / 2
//  def nextIfOdd(n: Long): Long = 3 * n + 1

//  def ifOdd(v: Long): Option[Long] = if (v / 2 == 0) None else Some(n)
//
//  def prev(n: Long): Long =
//    if (n > 1 && (n - 1) % 3 == 0) {
//      ((n - 1) / 3).max(n * 2)
//    } else {
//      n * 2
//    }

  def isEven(n: Long): Boolean = n % 2 == 0
  def isOdd(n: Long): Boolean = !isEven(n)

  def prev(n: Long, max: Long, chain: List[Long], ns: Set[Long]): List[Long] = {
//    println(n)
//    println(chain)

    if (n > max * 32 || ns.contains(n)) return chain

    val evenChain = prev(n * 2, max, n :: chain, ns + n)

    if (n > 1 && (n - 1) % 3 == 0) {
      val prevNIfOdd = (n - 1) / 3

      if (isOdd(prevNIfOdd)) {
        val oddChain = prev(prevNIfOdd, max, n :: chain, ns + n)

        if (evenChain.length > oddChain.length) {
          evenChain
        } else {
          oddChain
        }
      } else {
        evenChain
      }
    } else {
      evenChain
    }
  }

  @tailrec
  def next(n: Long, results: List[Long] = List()): List[Long] = {
    if (n == 1) return (n :: results).reverse

    val nextN = if (isEven(n)) {
      n / 2
    } else {
      3 * n + 1
    }

    next(nextN, n :: results)
  }

  def main(args: Array[String]) {
    val max = 1000000

//    val n = (0 to 100).map(1 << _).takeWhile(_ < max).reverse.head
//
//    println(n)

    val chain = prev(1, max, List[Long](), Set[Long]()).dropWhile(_ >= max)

    println(chain)
//    println(chain.sorted)
//    println(chain.filter(i => i % 3 == 0))
//
//    val foo = (max-1 to 0 by -1).dropWhile(i => i % 2 !=0 || i % 3 != 0).head
//
//    println(foo)
    println(chain.head)
    println(chain.length)

    val chain2 = next(chain.head)

    println(chain2.length)

    println(chain2)

    assert(chain.length == chain2.length)

    assert(!chain2.zip(chain).exists {
      case (n2, n1) => n1 != n2
    })
  }
}
