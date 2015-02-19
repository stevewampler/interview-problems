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
}
