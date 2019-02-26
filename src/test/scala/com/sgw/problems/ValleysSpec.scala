package com.sgw.problems

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class ValleysSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "The Valleys" should "correctly count the number of valleys" in {
    Valleys.countingValleys(8, "UDDDUDUU") should be (1)
    Valleys.countingValleys(12, "DDUUDDUDUUUD") should be (2)
  }
}
