package com.sgw.hackerrank.greedyalgos

import com.sgw.hackerrank.greedyalgos.{LuckBalance, MinAbsDifference}
import com.sgw.utils.Timer
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class LuckBalanceSpec extends FlatSpec with Matchers with BeforeAndAfter {
  val path = "/problems/hackerrank/greedyalgos/luckbalance"

  "The LuckBalance object" should "correctly calculate the luck of winning/losing contests" in {
    println {
      Timer {
        LuckBalance.run(
          this.getClass.getResourceAsStream(s"$path/Input00.txt")
        ) should be (
          29
        )
      }
    }

    println {
      Timer {
        LuckBalance.run(
          this.getClass.getResourceAsStream(s"$path/Input01.txt")
        ) should be (
          42
        )
      }
    }

    println {
      Timer {
        LuckBalance.run(
          this.getClass.getResourceAsStream(s"$path/Input02.txt")
        ) should be (
          21
        )
      }
    }
  }
}
