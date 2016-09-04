package com.sgw.problems

import org.scalatest.{FlatSpec, Matchers}

class IntSetSpec extends FlatSpec with Matchers {
  val e = Empty
  val s1 = Empty.incl(7).incl(5).incl(12).incl(4).incl(12).incl(1).incl(15).incl(20).incl(17).incl(2)
  val s2 = NonEmpty(7).incl(3).incl(22)

  "An empty set" should "not contain anything" in {
    e.contains(1) should be (false)
  }

  "The incl method" should "add a new item to a set" in {
    e incl 5 contains 5 should be (true)
  }

  "A set that contains 12" should "contain 12" in {
    println(s1)
    s1 contains 12 should be (true)
  }

  "A set's forall method" should "return true for an empty set" in {
    e.forall(x => false) should be (true)
  }

  "A set's forall method" should "return true if the predicate passes for all items in a non-empty set" in {
    s1.forall(x => true) should be (true)
  }

  "A set's forall method" should "return false if the predicate fails for at least one item in a non-empty set" in {
    s1.forall(_ != 12) should be (false)
  }

  "The union of two sets" should "contain all of the elements of both sets" in {
    val u = s1.union(s2)

    s1.forall(i => u.contains(i)) should be (true)
    s2.forall(i => u.contains(i)) should be (true)
  }
}
