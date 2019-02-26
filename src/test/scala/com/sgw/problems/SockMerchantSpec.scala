package com.sgw.problems

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class SockMerchantSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "The SockMerchant" should "correctly pair up socks" in {
    SockMerchant.sockMerchant(9, Array(10, 20, 20, 10, 10, 30, 50, 10, 20)) should be (3)
    SockMerchant.sockMerchant(5, Array(10, 20, 30, 40, 50)) should be (0)
    SockMerchant.sockMerchant(9, Array(10, 10, 10, 10, 10, 10, 10, 10, 10)) should be (4)
  }
}
