package com.sgw.coursera

object PascalsTriangle {
  /**
    * Returns the value at the specified column (zero based) and row (zero based)
    * within a Pascal Triangle. E.g.:
    *
    *         1
    *        1 1
    *       1 2 1
    *      1 3 3 1
    *     1 4 6 4 1
    *
    * E.g.
    *   col 0, row 2 => 1
    *   col 3, row 4 => 4
    *   col 4, row 4 => 1
    *
    * @param c the triangle's column index (zero based)
    * @param r the triangle's row index (zero based)
    *
    * @return the computed value of the triangle at the specified row and column.
    */
  def pascal(c: Int, r: Int): Int = {
    def go(c: Int, r: Int): Int =
      if (c > r) 0 else if (c == 0 || r == 0) 1 else {
        go(c - 1, r-1) + go(c, r - 1)
      }

    if (c < 0) throw new RuntimeException(s"Invalid input. c ($c) must be >= 0.")
    if (r < 0) throw new RuntimeException(s"Invalid input. r ($r) must be >= 0.")
    if (c > r) throw new RuntimeException(s"Invalid input. c ($c) must be <= r ($r).")

    go(c, r)
  }
}
