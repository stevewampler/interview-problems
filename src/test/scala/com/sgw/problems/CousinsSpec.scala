package com.sgw.problems

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class CousinsSpec extends FlatSpec with Matchers with BeforeAndAfter {
  val A = Cousins.Node("A")
  val B = Cousins.Node("B", A)
  val C = Cousins.Node("C", A)
  val D = Cousins.Node("D", B)
  val E = Cousins.Node("E", C)
  val F = Cousins.Node("F", D)
  val G = Cousins.Node("G", E)

  val X = Cousins.Node("X", G)
  val Y = Cousins.Node("Y", G)

  val H = Cousins.Node("H")
  val I = Cousins.Node("I", H)
  val J = Cousins.Node("J", H)

  "The Cousins.relationship function" should "return None for unrelated nodes" in {
    Cousins.relationship(F, I) should be (None)
  }

  "The Cousins.relationship function" should "correctly calculate the relationship between two related nodes" in {
    Cousins.relationship(B, C) should be (Some((0, 0)))
    Cousins.relationship(F, E) should be (Some((1, 1)))
    Cousins.relationship(F, Y) should be (Some((2, 1)))
    Cousins.relationship(D, Y) should be (Some((1, 2)))
  }
}
