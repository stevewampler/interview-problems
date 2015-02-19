package com.sgw.problems

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

class ImageObjectCounterSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "The image object counter" should "count zero objects in a blank image" in {
    val image = Array(
      Array( 0, 0, 0, 0, 0, 0 ),
      Array( 0, 0, 0, 0, 0, 0 ),
      Array( 0, 0, 0, 0, 0, 0 ),
      Array( 0, 0, 0, 0, 0, 0 ),
      Array( 0, 0, 0, 0, 0, 0 ),
      Array( 0, 0, 0, 0, 0, 0 ),
      Array( 0, 0, 0, 0, 0, 0 ),
      Array( 0, 0, 0, 0, 0, 0 ),
      Array( 0, 0, 0, 0, 0, 0 ),
      Array( 0, 0, 0, 0, 0, 0 )
    )

    ImageObjectCounter.countObjects(image) should be (0)
  }

  it should "count zero objects a zero sized image" in {
    val image = Array.ofDim[Int](0, 0)

    ImageObjectCounter.countObjects(image) should be (0)
  }

  it should "correctly count the objects in a image with objects" in {
    val image = Array(
      Array( 0, 0, 0, 0, 1, 0 ),
      Array( 0, 1, 0, 1, 1, 1 ),
      Array( 0, 0, 0, 0, 1, 0 ),
      Array( 1, 1, 1, 0, 0, 0 ),
      Array( 1, 1, 1, 0, 1, 0 ),
      Array( 1, 1, 1, 0, 0, 1 ),
      Array( 0, 0, 0, 0, 1, 0 )
    )

    ImageObjectCounter.countObjects(image) should be (4)
  }
}
