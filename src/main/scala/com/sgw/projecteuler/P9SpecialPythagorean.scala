package com.sgw.projecteuler

import scala.annotation.tailrec

/**
 *
 * A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 *
 * a^2 + b^2 = c^2
 * For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
 *
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 *
 * Find the product abc.
 */
object P9SpecialPythagorean {
  def findSpecialPythagoreanTriplet(a: Long = 1, b: Long = 2): Option[(Long, Long, Long)] = {
    if (b > 1000) return None
    if (a >= b) return findSpecialPythagoreanTriplet(1, b + 1)

    val cDouble = Math.sqrt(a*a + b*b)

    val c = cDouble.toInt

    if (c == cDouble && b < c && a + b + c == 1000) return Some((a, b, c.toLong))

    findSpecialPythagoreanTriplet(a + 1, b)
  }

  def main(args: Array[String]) {
    val (a, b, c, sum, prod) = findSpecialPythagoreanTriplet().map {
      case (a, b, c) => (a, b, c, a + b + c, a * b * c)
    }.getOrElse(throw new RuntimeException("Failed to find a special pythagorean triplet."))

    println(s"a,b,c = $a,$b,$c")
    println(s"sum = $sum")
    println(s"product = $prod")

    assert(a == 200)
    assert(b == 375)
    assert(c == 425)
    assert(sum == 1000)
    assert(prod == 31875000)
  }
}
