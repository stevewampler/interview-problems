package com.sgw.projecteuler

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

class P012HighlyDivisibleTriangularNumberSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "The highly divisible triangular number solution" should "return the correct triangular number with over 5 factors" in {
    P012HighlyDivisibleTriangularNumber.solve(5) should be(28)
  }

  "The highly divisible triangular number solution" should "return the correct triangular number with over 500 factors" in {
    P012HighlyDivisibleTriangularNumber.solve(500) should be(76576500)
  }
}
