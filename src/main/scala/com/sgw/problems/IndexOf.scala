package com.sgw.problems

import scala.annotation.tailrec

/**
 * Find the index of a target string within a source string.
 */
object IndexOf {
  def indexOf(str: String, trg: String): Int = {
    var i = 0

    while(i <= str.size - trg.size) {
      var j = 0

      while (j < trg.size && str(i + j) == trg(j)) {
        j = j + 1
      }

      if (j == trg.size) {
        return i
      }

      i = i + 1
    }

    -1
  }

  @tailrec
  def startsWith(str: String, trg: String): Boolean =
    trg.isEmpty || (trg.size <= str.size && str.head == trg.head && startsWith(str.tail, trg.tail))

  @tailrec
  def indexOf2(str: String, trg: String, index: Int = 0): Int = {
    if (str.size < trg.size) return -1

    if (!startsWith(str, trg)) {
      indexOf2(str.tail, trg, index + 1)
    } else {
      index
    }
  }
}
