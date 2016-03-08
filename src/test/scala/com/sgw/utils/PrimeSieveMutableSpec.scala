package com.sgw.utils

import com.sgw.problems.PrimeSieveMutable
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class PrimeSieveMutableSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "A PrimeSieveMutable" should "find all of the prime numbers between 2 and 100" in {
    val expected = Seq(
      2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
    )

    val actual = PrimeSieveMutable.findPrimes(100)

    actual should be (expected)
  }
}
