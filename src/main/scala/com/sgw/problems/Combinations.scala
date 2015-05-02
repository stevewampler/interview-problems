package com.sgw.problems

object Combinations extends App {
  val values = List('A', 'B', 'C', 'D')

  /**
   * Generate all of the combinations of a list of characters:
A
AB
ABC
ABCD
ABD
AC
ACD
AD
B
BC
BCD
BD
C
CD
D
    */
  def combinations(accum: String, level: Int, values: List[Char]): Unit = {
    var i = level

    while (i < values.length) {
      val newAccum = accum + values(i) // loop adds the "B" to "A" for example

      println(newAccum)

      combinations(newAccum, i + 1, values) // recurse to add the "C" and "D" combinations to "AB" for example

      i = i + 1
    }
  }


  combinations("", 0, values)

  println()

  def combinations2(accum: String, values: List[Char], result: String = ""): String =
    values.zipWithIndex.foldLeft(result) {
      case (result2, (v, i)) => {
        val newAccum = accum + v // loop adds the "B" to "A" for example
        val newResult = result2 + newAccum + "\n"

        combinations2(newAccum, values.drop(i + 1), newResult) // recurse to add the "C" and "D" combinations to "AB" for example
      }
    }

  println(combinations2("", values))
}
