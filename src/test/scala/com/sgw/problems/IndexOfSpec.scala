package com.sgw.problems

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class IndexOfSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "The startsWith method" should "correctly indicate whether or not a string starts with another string" in {
    IndexOf.startsWith("abcdefg", "") should be (right = true)
    IndexOf.startsWith("abcdefg", "a") should be (right = true)
    IndexOf.startsWith("abcdefg", "abc") should be (right = true)
    IndexOf.startsWith("abcdefg", "bcd") should be (right = false)
    IndexOf.startsWith("abcdefg", "abcdefg") should be (right = true)
    IndexOf.startsWith("abcdefg", "abcdefgh") should be (right = false)
  }

  "The indexOf method" should "correctly find a string within a string" in {
    IndexOf.indexOf("abcdefgh", "cde") should be (2)
    IndexOf.indexOf("abcdefgh", "abcdefgh") should be (0)
    IndexOf.indexOf("abcdefgh", "abc") should be (0)
    IndexOf.indexOf("abcdefgh", "fgh") should be (5)
    IndexOf.indexOf("abcdefgh", "a") should be (0)
    IndexOf.indexOf("abcdefgh", "c") should be (2)
    IndexOf.indexOf("abcdefgh", "h") should be (7)
    IndexOf.indexOf("abcdefgh", "z") should be (-1)
    IndexOf.indexOf("abcdefgh", "abcdefgh") should be (0)
    IndexOf.indexOf("abcdefgh", "abcdefghi") should be (-1)
  }

  "The indexOf2 method" should "correctly find a string within a string" in {
    IndexOf.indexOf2("abcdefgh", "cde") should be (2)
    IndexOf.indexOf2("abcdefgh", "abcdefgh") should be (0)
    IndexOf.indexOf2("abcdefgh", "abc") should be (0)
    IndexOf.indexOf2("abcdefgh", "fgh") should be (5)
    IndexOf.indexOf2("abcdefgh", "a") should be (0)
    IndexOf.indexOf2("abcdefgh", "c") should be (2)
    IndexOf.indexOf2("abcdefgh", "h") should be (7)
    IndexOf.indexOf2("abcdefgh", "z") should be (-1)
    IndexOf.indexOf2("abcdefgh", "abcdefgh") should be (0)
    IndexOf.indexOf2("abcdefgh", "abcdefghi") should be (-1)
    IndexOf.indexOf2("abcdefgh", "abcdefgi") should be (-1)
    IndexOf.indexOf2("aaaaaaab", "ab") should be (6)
    IndexOf.indexOf2("aaaaaaab", "aaac") should be (-1)
  }
}
