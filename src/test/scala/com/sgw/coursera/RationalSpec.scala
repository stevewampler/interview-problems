package com.sgw.coursera

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by swampler on 8/28/16.
  */
class RationalSpec extends FlatSpec with Matchers {
  val r1 = Rational(1, 3)
  val r2 = Rational(5, 7)
  val r3 = Rational(3, 2)
  val r4 = Rational(10, 100)

  "The Rational constructor" should "throw an exception if the denominator <= 0" in {
    try {
      Rational(1, 0)
      fail("Should have thrown an IllegalArgumentException!")
    } catch {
      case ex: IllegalArgumentException => // expected
      case ex: Throwable => fail("Should have thrown an IllegalArgumentException", ex)
    }
  }

  "The Rational constructor" should "simplify the rational" in {
    println(r4)
    r4.numer should be (1)
    r4.denom should be (10)
  }

  "The < operator" should "return true if the first rational is less-than the second" in {
    r1 < r2 should be (true)
  }

  "The <= operator" should "return true if the first rational is less-than or equal to the second" in {
    r1 < r2 should be (true)
    r1 <= r1 should be (true)
  }

  "The > operator" should "return true if the first rational is less-than the second" in {
    r2 > r1 should be (true)
  }

  "The >= operator" should "return true if the first rational is less-than the second" in {
    r2 >= r1 should be (true)
    r2 >= r2 should be (true)
  }

  "The min method" should "return the minimum of two rationals" in {
    r1.min(r2) should be (r1)
  }

  "The max method" should "return the maximum of two rationals" in {
    r1.max(r2) should be (r2)
  }

  "Negating a Rational" should "produce the correct value" in {
    val value = -r1

    println(value)

    value should be (Rational(-1, 3))
  }

  "Inverting a Rational" should "produce the correct value" in {
    val value = r1.invert

    println(value)

    value should be (Rational(3, 1))
  }

  "Adding two Rationals" should "produce the correct value" in {
    val value = r1 + r2

    println(value)

    value should be (Rational(22, 21))
  }

  "Subtracting two Rationals" should "produce the correct value" in {
    val value = r1 - r2

    println(value)

    value should be (Rational(-8, 21))
  }

  "Multiplying two Rationals" should "produce the correct value" in {
    val value = r1 * r2

    println(value)

    value should be (Rational(5, 21))
  }

  "Dividing two Rationals" should "produce the correct value" in {
    val value = r1 / r2

    println(value)

    value should be (Rational(7, 15))
  }

  "Subtracting two Rationals from another" should "roduce the correct value" in {
    val value = r1 - r2 - r3

    println(value)

    value should be (Rational(-79, 42))
  }
}
