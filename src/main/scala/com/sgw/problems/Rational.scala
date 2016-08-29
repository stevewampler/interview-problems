package com.sgw.problems

object Rational {
  def apply(numer: Int, denom: Int = 1): Rational = {
    require(denom > 0, "denominator must be positive")

    val g = GreatestCommonDenominator.gcd(numer.abs, denom)

    new Rational(numer / g, denom / g)
  }
}

/**
  * A class that represents rational numbers.
  */
class Rational(n: Int, d: Int) {
  val numer = n
  val denom = d

  def negate: Rational = Rational(-numer, denom)

  def unary_- : Rational = negate

  def invert: Rational = if (numer < 0)
    Rational(-denom, -numer)
  else
    Rational(denom, numer)

  def lt(that: Rational): Boolean = numer * that.denom < that.numer * denom
  def lte(that: Rational): Boolean = numer * that.denom <= that.numer * denom
  def gt(that: Rational): Boolean = numer * that.denom > that.numer * denom
  def gte(that: Rational): Boolean = numer * that.denom >= that.numer * denom

  def <(that: Rational): Boolean = lt(that)
  def <=(that: Rational): Boolean = lte(that)
  def >(that: Rational): Boolean = gt(that)
  def >=(that: Rational): Boolean = gte(that)

  def min(that: Rational): Rational = if (lt(that)) {
    this
  } else {
    that
  }

  def max(that: Rational): Rational = if (gt(that)) {
    this
  } else {
    that
  }

  def add(that: Rational): Rational = Rational(
    numer * that.denom + that.numer * denom,
    denom * that.denom
  )

  def subtract(that: Rational): Rational = add(that.negate)

  def multiply(that: Rational): Rational = Rational(
    numer * that.numer,
    denom * that.denom
  )

  def divide(by: Rational): Rational = multiply(by.invert)

  def +(that: Rational): Rational = add(that)
  def -(that: Rational): Rational = subtract(that)
  def *(that: Rational): Rational = multiply(that)
  def /(that: Rational): Rational = divide(that)

  override def toString = s"$numer/$denom"

  override def equals(o: Any) = o match {
    case that: Rational => numer * that.denom == that.numer * denom
    case _ => false
  }

  override def hashCode = numer.hashCode + denom.hashCode()
}
