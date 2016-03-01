package com.sgw.problems

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

class RobotSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "A Robot that only turns" should "be in a circle at the origin of radius 1" in {
    Robot().move("LRLRRRR").isInCircle(0, 0, 1.0) should be(true)
  }

  "A Robot that ends up in negative space" should "be outside a circle at the origin of radius 1" in {
    Robot().move("GLGR").isInCircle(0, 0, 1.0) should be(false)
  }

  "A Robot that only moves diagonally 5 spaces" should "be circle at 5,5 of radius 5" in {
    Robot().move("GRGLGRGLGRGLGRGLGRGL").isInCircle(5, 5, 5.0) should be(true)
  }

  "A Robot that only moves in a square" should "be in a circle at the origin of radius 1" in {
    Robot().move("GGGGGLGGGGGLGGGGGLGGGGG").isInCircle(0, 0, 1.0) should be(true)
  }
}
