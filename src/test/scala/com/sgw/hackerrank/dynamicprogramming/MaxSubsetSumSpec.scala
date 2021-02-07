package com.sgw.hackerrank.dynamicprogramming

import com.sgw.hackerrank.dynamicprogramming.MaxSubsetSum
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

import scala.util.Success

class MaxSubsetSumSpec extends FlatSpec with Matchers with BeforeAndAfter {
  val path = "/problems/hackerrank/dynamicprogramming/maxsubsetsum"

  "The MaxSubsetSum object" should "correctly calculate max sum of a subset of an array" in {
    MaxSubsetSum.run(
      this.getClass.getResourceAsStream(s"$path/SmallTestInput00.txt")
    ) should be (
      Success(15)
    )

    MaxSubsetSum.run(
      this.getClass.getResourceAsStream(s"$path/SmallTestInput01.txt")
    ) should be (
      Success(16)
    )

    MaxSubsetSum.run(
      this.getClass.getResourceAsStream(s"$path/SmallTestInput02.txt")
    ) should be (
      Success(20)
    )

    MaxSubsetSum.run(
      this.getClass.getResourceAsStream(s"$path/SmallTestInput03.txt")
    ) should be (
      Success(1)
    )

    MaxSubsetSum.run(
      this.getClass.getResourceAsStream(s"$path/SmallTestInput04.txt")
    ) should be (
      Success(9)
    )

    MaxSubsetSum.run(
      this.getClass.getResourceAsStream(s"$path/SmallTestInput05.txt")
    ) should be (
      Success(2)
    )

    MaxSubsetSum.run(
      this.getClass.getResourceAsStream(s"$path/SmallTestInput06.txt")
    ) should be (
      Success(4)
    )

    MaxSubsetSum.run(
      this.getClass.getResourceAsStream(s"$path/TestInput01.txt")
    ) should be (
      Success(7412694)
    )

    MaxSubsetSum.run(
      this.getClass.getResourceAsStream(s"$path/TestInput07.txt")
    ) should be (
      Success(15142193)
    )
  }
}
