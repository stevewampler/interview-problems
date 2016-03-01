package com.sgw.problems

import java.util.concurrent.TimeUnit

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

class CountSquaredIntegersSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "CountSquaredIntegers" should "return the correct number of squared integers" in {
    val lines = Array(
      "6",
      "1 10",
      "3 9",
      "17 24",
      "17 25",
      "17 26",
      "16 25"
    )

    val result = CountSquaredIntegers.countSquaredIntegers(lines)

    result should be(Array(3, 2, 0, 1, 1, 2))
  }

  "CountSquaredIntegers" should "return the correct number of squared integers in a huge range in a reasonable amount of time" in {
    import scala.concurrent.Future
    import ExecutionContext.Implicits.global

    val lines = Array(
      "1",
      s"1 ${Int.MaxValue}"
    )

    val result = Future { CountSquaredIntegers.countSquaredIntegers(lines) }

    Await.result(result, Duration.apply(5, TimeUnit.SECONDS)) should be(Array(46340))
  }
}
