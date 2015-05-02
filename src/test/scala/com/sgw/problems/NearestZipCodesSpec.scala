package com.sgw.problems

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

class NearestZipCodesSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "The NearestZipCodes class" should "return the nearest n zip codes to the specified zip code" in {
    val nearestZipCodes = ZipCodes.nearest("01505", 5)

    nearestZipCodes.foreach(println)

    nearestZipCodes.size should be (5)
  }
}
