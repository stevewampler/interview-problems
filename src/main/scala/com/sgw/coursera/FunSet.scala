package com.sgw.coursera

import scala.annotation.tailrec

object FunSet {
  /**
    * Returns the set of the one given element.
    */
  def apply(elem: Int): FunSet = FunSet((v: Int) => v == elem)

  /**
    * Prints the contents of a set on the console.
    */
  def printSet(s: FunSet) {
    println(s.toString)
  }
}

/**
  * An integer set defined using only a "contains" function. Adapted from the Coursera's "Functional Programming
  * Principles in Scala" course.
  *
  * @param contains a function defining the contents of the set that takes an integer and returns a boolean indicating
  *                 whether or not the integer is within the set
  * @param bounds the +/- bounds for some of the methods
  */
case class FunSet(contains: Int => Boolean, bounds: Int = 1000) {
  /**
    * Returns the union of this set and the specified set `t`.
    * I.e. the sets of all elements that are in either this set or `t`.
    */
  def union(t: FunSet): FunSet = FunSet((v: Int) => contains(v) || t.contains(v))

  /**
    * Returns the intersection of this set and the specified set `t`.
    * I.e. the set of all elements that are both in this set and `t`.
    */
  def intersect(t: FunSet): FunSet = FunSet((v: Int) => contains(v) && t.contains(v))

  /**
    * Returns a set containing the difference of this set and the specified set.
    * I.e. the set of all elements of this set that are not in `t`.
    */
  def diff(t: FunSet): FunSet = FunSet((v: Int) => contains(v) && !t.contains(v))

  /**
    * Returns the subset of this set for which `p` holds.
    */
  def filter(p: Int => Boolean): FunSet = intersect(FunSet(p, bounds))

  /**
    * Returns whether all bounded integers within this set satisfy `p`.
    */
  def forall(p: Int => Boolean): Boolean = {
    @tailrec
    def iter(a: Int): Boolean =
      if (a > bounds) true
      else if (contains(a)) p(a) && iter(a + 1)
      else iter(a + 1)

    iter(-bounds)
  }

  /**
    * Returns whether there exists a bounded integer within this set
    * that satisfies `p`.
    */
  def exists(p: Int => Boolean): Boolean = {
    @tailrec
    def iter(a: Int): Boolean =
      if (a > bounds) false
      else if (contains(a)) p(a) || iter(a + 1)
      else iter(a + 1)

    iter(-bounds)
  }

  /**
    * Returns a set transformed by applying `f` to each element of this set.
    */
  def map(f: Int => Int): FunSet = FunSet((v: Int) => exists((x: Int) => f(x) == v))

  /**
    * Generates a string representation of this set within this set's bounds.
    */
  override def toString: String = {
    val xs = for (i <- -bounds to bounds if contains(i)) yield i
    xs.mkString("{", ",", "}")
  }
}
