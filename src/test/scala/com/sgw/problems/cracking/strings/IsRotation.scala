package com.sgw.problems.cracking.strings

// write a function that will determine if one string is a rotation of another using only one call to isSubstring
object IsRotation {

  // is s1 a rotation of s2?
  def isRotation(s1: String, s2: String): Boolean = {
    IsSubstring.isSubstring(s1, s2 + s2)
  }

  def main(args: Array[String]): Unit = {
    assert( isRotation("cdefab", "abcdef"))
    assert(!isRotation("cdefab", "abcdefg"))
  }
}
