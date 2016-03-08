package com.sgw.utils

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

class FactorizationSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "The primeFactors method" should "return the correct prime factors of a number" in {
    Factorization.primeFactors(56) should be(List(2, 2, 2, 7, 14, 28))

    Factorization.primeFactors(57) should be(List(3, 19))

    Factorization.primeFactors(31) should be(List())
  }

  "The factors method" should "return the correct factors of a number" in {
    Factorization.factors(56).sorted should be(List(1, 2, 4, 7, 8, 14, 28, 56))

    Factorization.factors(57).sorted should be(List(1, 3, 19, 57))

    Factorization.factors(31).sorted should be(List(1, 31))
  }
}
