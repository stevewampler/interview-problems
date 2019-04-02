package com.sgw.cracking.strings

object IsSubstring {
  // is s1 a substring of s2?
  // mutable
  def isSubstringMutable(s1: String, s2: String): Boolean = {
    val l1 = s1.length
    val l2 = s2.length

    if (l1 > l2) return false
    if (l1 == l2) return s1 == s2

    // s1 is the shorter string

    var i2 = 0

    // loop over the characters of s2 to find a substring that matches s1
    while (i2 < l2) {
      var i1  = 0
      var i2Prime = i2

      // loop over both string's as long as the characters match
      while (i1 < l1 && i2Prime < l2 && s1.charAt(i1) == s2.charAt(i2Prime)) {
        i1 = i1 + 1
        i2Prime = i2Prime + 1
      }

      // if we consumed all of s1, then we found a match
      if (i1 == l1) return true

      i2 = i2 + 1
    }

    return false
  }

  // is s1 a substring of s2?
  // non-mutable
  def isSubstringNonMutable(s1: String, s2: String): Boolean = {
    val l1 = s1.length
    val l2 = s2.length

    if (l1 > l2) return false
    if (l1 == l2) return s1 == s2

    // s1 is the shorter string

    val foo = (0 until l2).dropWhile { i2 =>
      val str = (0 until l1).map { i1 =>
        s1.charAt(i1) -> s2.charAt(i2 + i1)
      }.takeWhile { case (c1, c2) =>
        c1 == c2
      }

      str.length != l1
    }

    foo.nonEmpty
  }

  def isSubstring(s1: String, s2: String): Boolean = isSubstringMutable(s1, s2)

  def main(args: Array[String]): Unit = {
    assert( isSubstring("", ""))
    assert( isSubstring("", "abc"))
    assert( isSubstring("ab", "abc"))
    assert( isSubstring("a", "abc"))
    assert( isSubstring("b", "abc"))
    assert( isSubstring("c", "abc"))
    assert(!isSubstring("d", "abc"))
    assert(!isSubstring("abc", "ab"))
    assert( isSubstring("skateboard", "rdskateboardskateboa"))
  }
}
