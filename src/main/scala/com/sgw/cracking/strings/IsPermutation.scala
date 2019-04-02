package com.sgw.cracking.strings

// write a function that will return true if a string is a permutation of another string
object IsPermutation {

  def isPermutation(str: String, of: String): Boolean = {
    if (str.length != of.length) return false

    str.toSet == of.toSet
  }

  def main(args: Array[String]): Unit = {
    assert( isPermutation("abc", "abc"))
    assert( isPermutation("abc", "cba"))
    assert(!isPermutation("abcd","dba"))
    assert(!isPermutation("abcc","abc"))
  }
}
