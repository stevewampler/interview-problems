package com.sgw.problems

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalTriangleSuite extends FunSuite {
  import com.sgw.problems.PascalsTriangle.pascal

  test("pascal: col=0,row=2") {
    assert(pascal(0, 2) === 1)
  }

  test("pascal: col=1,row=2") {
    assert(pascal(1, 2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1, 3) === 3)
  }

  test("pascal: col=3,row=4") {
    assert(pascal(3, 4) === 4)
  }

  test("pascal: col=4,row=4") {
    assert(pascal(4, 4) === 1)
  }

  test("pascal: col < 0 error") {
    try {
      pascal(-1, 2)
      fail("Should have thrown an exception.")
    } catch {
      case ex => assert(ex.getMessage == "Invalid input. c (-1) must be >= 0.")
    }
  }

  test("pascal: row < 0 error") {
    try {
      pascal(0, -1)
      fail("Should have thrown an exception.")
    } catch {
      case ex => assert(ex.getMessage == "Invalid input. r (-1) must be >= 0.")
    }
  }

  test("pascal: col > row error") {
    try {
      pascal(5, 2)
      fail("Should have thrown an exception.")
    } catch {
      case ex => assert(ex.getMessage == "Invalid input. c (5) must be <= r (2).")
    }
  }
}
