package com.sgw.problems

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

class WaterWallSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "The HoldWater1D class" should "return zero for a wall that won't hold any water" in {
    WaterWall.canHold(Seq(0, 1, 2, 3, 3, 2, 1, 0)) should be (0)
  }

  it should "return the correct amount of water for a wall that will hold water" in {
    WaterWall.canHold(Seq(0, 2, 0, 1, 1, 3, 1, 2, 1)) should be (5)
    WaterWall.canHold(Seq(3, 2, 1, 0, 1, 2, 3)) should be (9)
  }
}
