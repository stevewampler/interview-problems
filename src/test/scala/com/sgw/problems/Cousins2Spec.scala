package com.sgw.problems

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class Cousins2Spec extends FlatSpec with Matchers with BeforeAndAfter {
  val A = Cousins2.Node("A")
  val B = Cousins2.Node("B", A)
  val C = Cousins2.Node("C", A)
  val D = Cousins2.Node("D", B)
  val E = Cousins2.Node("E", C)
  val F = Cousins2.Node("F", D)
  val G = Cousins2.Node("G", E)

  val X = Cousins2.Node("X", G)
  val Y = Cousins2.Node("Y", G)

  val H = Cousins2.Node("H")
  val I = Cousins2.Node("I", H)
  val J = Cousins2.Node("J", H)

  "The Cousins2.relationship function" should "return None for unrelated nodes" in {
    Cousins2.relationship(F, I) should be (None)
  }

  "The Cousins2.relationship function" should "correctly calculate the relationship between two related nodes" in {
    Cousins2.relationship(B, C) should be (Some((0, 0)))
    Cousins2.relationship(F, E) should be (Some((1, 1)))
    Cousins2.relationship(F, Y) should be (Some((2, 1)))
    Cousins2.relationship(D, Y) should be (Some((1, 2)))
  }
}
