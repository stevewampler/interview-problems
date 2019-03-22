package com.sgw.problems.cracking.strings

// write a function that determines if a string is the permutation of a palindrome (e.g. tococat)
// the palindrome does not need to be a dictionary word
object IsPalindromePermutation {

  def isPalindromePermutation(str: String): Boolean = {
    val map = str.map { c =>
      c -> 1
    }.groupBy { case (c, _) =>
      c
    }.map { case (c, list) =>
      c -> list.size
    }.filter { case (c, count) =>
        count % 2 != 0
    }

    println(map)

    map.isEmpty || map.size == 1
  }

  def main(args: Array[String]): Unit = {
    assert( isPalindromePermutation(""))
    assert( isPalindromePermutation("tacocat"))
    assert( isPalindromePermutation("ttacocatt"))
    assert( isPalindromePermutation("ttacoocatt"))
    assert( isPalindromePermutation("cattaco"))
    assert( isPalindromePermutation("foo"))
    assert( isPalindromePermutation("aaa"))
    assert( isPalindromePermutation("aaaa"))
    assert( isPalindromePermutation("abba"))
    assert( isPalindromePermutation("baba"))
    assert( isPalindromePermutation("abcccab"))
    assert(!isPalindromePermutation("abccc"))
    assert(!isPalindromePermutation("abccde"))
  }
}
