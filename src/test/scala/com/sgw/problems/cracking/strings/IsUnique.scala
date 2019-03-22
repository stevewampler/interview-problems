package com.sgw.problems.cracking.strings

// write a function that will determine if a string contains all unique characters
object IsUnique {

  def isUnique1(str: String): Boolean = {
    if (str.length < 2) return true

    val chars = str.sorted.toList

    chars.zip(chars.tail).dropWhile { case (c1, c2) =>
      c1 != c2
    }.isEmpty
  }

  def isUnique(str: String): Boolean = str.length == str.distinct.length

  def main(args: Array[String]): Unit = {
    assert( isUnique(""))
    assert( isUnique("a"))
    assert( isUnique("ab"))
    assert( isUnique("abc"))
    assert(!isUnique("abcb"))
  }
}
