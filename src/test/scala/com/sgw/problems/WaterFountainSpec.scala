package com.sgw.problems

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

class WaterFountainSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "A WaterFountain" should "not hold any water if it has a leak" in {
    val fountain = Array(
      Array(3,2,0,1),
      Array(3,0,0,2),
      Array(3,2,0,1),
      Array(2,2,2,2)
    )

    WaterFountain(fountain).volume should be (0)
  }

  it should "hold water if the fountain doesn't have a leak" in {
    val fountain = Array(
      Array(3,2,1,1),
      Array(3,0,0,2),
      Array(3,2,0,1),
      Array(2,2,2,2)
    )

    WaterFountain(fountain).volume should be (3)

    val fountain2 = Array(
      Array(3,3,3,3),
      Array(3,0,0,3),
      Array(3,0,0,3),
      Array(3,3,3,3)
    )

    WaterFountain(fountain2).volume should be (12)

    val fountain3 = Array(
      Array(3,3,3,3),
      Array(3,1,1,3),
      Array(3,1,1,3),
      Array(3,3,3,3)
    )

    WaterFountain(fountain3).volume should be (8)

    val fountain4 = Array(
      Array(3,3,3,3,3,3,3),
      Array(3,0,0,2,0,0,3),
      Array(3,0,0,3,0,0,3),
      Array(3,2,3,3,3,2,3),
      Array(3,0,0,3,0,0,3),
      Array(3,0,0,2,0,0,3),
      Array(3,3,3,3,3,3,3)
    )

    WaterFountain(fountain4).volume should be (12*4+4)

    val fountain5 = Array(
      Array(3,3,3,3,3,3,3),
      Array(3,0,0,2,0,0,3),
      Array(3,0,0,3,0,0,3),
      Array(3,2,3,3,3,2,3),
      Array(3,0,0,3,0,0,3),
      Array(3,0,0,2,0,0,3),
      Array(3,3,3,3,0,3,3)
    )

    WaterFountain(fountain5).volume should be (8*3)
  }

  it should "not leak water diagonally" in {
    val fountain = Array(
      Array(3,2,1,0),
      Array(3,0,0,2),
      Array(3,2,0,1),
      Array(2,2,2,2)
    )

    WaterFountain(fountain).volume should be (3)

    val fountain2 = Array(
      Array(0,3,3,0),
      Array(3,0,0,3),
      Array(3,0,0,3),
      Array(0,3,3,0)
    )

    WaterFountain(fountain2).volume should be (12)
  }
}
