package com.sgw.problems.hackerrank.greedyalgos

import com.sgw.hackerrank.greedyalgos.MinAbsDifference
import com.sgw.utils.Timer
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class MinAbsDifferenceSpec extends FlatSpec with Matchers with BeforeAndAfter {
  val path = "/problems/hackerrank/greedyalgos/minabsdifference"

  "The MinAbsDifference object" should "correctly calculate the minimum absolute difference of an array of ints" in {
    println {
      Timer {
        MinAbsDifference.run(
          this.getClass.getResourceAsStream(s"$path/MinAbsDifferenceSpecInput00.txt")
        ) should be (
          3
        )
      }
    }

    println {
      Timer {
        MinAbsDifference.run(
          this.getClass.getResourceAsStream(s"$path/MinAbsDifferenceSpecInput01.txt")
        ) should be (
          1
        )
      }
    }

    println {
      Timer {
        MinAbsDifference.run(
          this.getClass.getResourceAsStream(s"$path/MinAbsDifferenceSpecInput02.txt")
        ) should be (
          3
        )
      }
    }

    println {
      Timer {
        MinAbsDifference.run(
          this.getClass.getResourceAsStream(s"$path/MinAbsDifferenceSpecTestInput02.txt")
        ) should be (
          0
        )
      }
    }
  }
}
