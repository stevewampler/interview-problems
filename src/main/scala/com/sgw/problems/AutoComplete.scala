package com.sgw.problems

import com.sgw.collections.immutable
import com.sgw.collections.immutable.Trie

/**
  * Given a set of potential search result strings and a prefix string, return the strings that could auto-complete
  * the prefix.
  *
  * Solution: Use a "Trie" (https://en.wikipedia.org/wiki/Trie)
  */
object AutoComplete {
  def autoComplete(strs: List[String], prefix: String): List[String] = immutable.Trie(strs).complete(prefix)

  def main(args: Array[String]): Unit = {
    val strs = List(
      "austin",
      "alburn",
      "allentown",
      "charlotte",
      "boston",
      "bottom",
      "boss",
      "barbie",
      "barb"
    )

    val trie = immutable.Trie(strs)

    println(trie)

    println("al ...")
    println(trie.complete("al"))
    println("b ...")
    println(trie.complete("b"))
    println("bo ...")
    println(trie.complete("bo"))
  }
}
