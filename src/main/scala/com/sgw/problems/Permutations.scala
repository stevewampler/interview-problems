package com.sgw.problems

import com.sgw.utils.Factorial

/**
  * Generate the permutations of the characters of a string.
  */
object Permutations {
  // distributes c throughout the specified strings
  def distribute(c: Char, strings: Set[String]): Set[String] = {
    strings.flatMap { str =>
      (0 to str.length).foldLeft(Set[String]()) { case (acc, i) =>
        // chop the string into left and right parts at the ith character
        val left = str.substring(0, i)
        val right = str.substring(i)

        // create a new string with the character c inserted between the left and right parts of the current string
        // and add the result to the accumulator
        acc + s"$left$c$right"
      }
    }
  }

  def permutations(str: String): Set[String] = {
    val len = str.length

    if (len == 0) return Set.empty

    if (len == 1) return Set(str)

    // generate the permutations of the tail of the string and then distribute the head character into the resulting permutations
    distribute(str.head, permutations(str.tail))
  }

  def main(args: Array[String]): Unit = {
    {
      val expected = Set(
        "ab",
        "ba"
      )
      val actual = permutations("ab")

      println(expected)
      println(actual)

      assert(actual == expected)
    }

    {
      val expected = Set(
        "abc",
        "bac",
        "bca",
        "acb",
        "cab",
        "cba"
      )
      val actual = permutations("abc")

      println(expected)
      println(actual)

      assert(actual == expected)
    }

    val r4 = permutations("abcd").toList.sorted
    assert(r4.size == Factorial.factorial(4))
    println(r4)
  }
}
