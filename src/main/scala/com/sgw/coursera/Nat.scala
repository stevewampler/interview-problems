package com.sgw.coursera

/**
  * Can all of the Scala primitive types be implemented as objects without resorting to the scala or java primitive types?
  *
  * In this case, implement a class that represents all natural numbers (integers >= 0).
  *
  * From the Coursera Functional Programming in Scala course.
  *
  * These are call Peano numbers.
  */
abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat
  def successor: Nat = Succ(this)

  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true

  def predecessor: Nat = throw new Error("0 has no predecessor")

  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if (that.isZero) Zero else throw new Error("0 has no predecessor")
}

case class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false

  def predecessor: Nat = n

  def + (that: Nat): Nat = Succ(n + that)
  def - (that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}
