package com.sgw.problems

object PrintSpiral extends App {
  val m = Array(
    Array(1, 2, 3, 4, 5),
    Array(6, 7, 8, 9, 10),
    Array(11, 12, 13, 14, 15)
  )

  def next(nr: Int, nc: Int, r: Int, c: Int, dir: Int) = {
    dir match {
      // left to right
      case 0 if c == (nc - 1) - r =>
        // end of row, so go down now
        (r + 1, c, 1)
      case 0 =>
        (r, c + 1, 0)

      // top to bottom
      case 1 if r == (nr - 1) - (nc - (c + 1)) =>
        // end of column, so go r to l
        (r, c - 1, 2)
      case 1 =>
        (r + 1, c, 1)

      // right to left
      case 2 if c == (nr - 1) - r =>
        (r - 1, c, 3)
      case 2 =>
        (r, c - 1, 2)

      // bottom to top
      case 3 if r == c + 1 =>
        (r, c + 1, 0)

      case 3 =>
        (r - 1, c, 3)
    }
  }

  def printSpiral(arr: Array[Array[Int]]) = {
    val nr = arr.length
    val nc = arr(0).length

    (0 until (nr * nc)).foldLeft(((0, 0, 0), List[Int]())) { case (((r, c, dir), acc), i) =>
      (next(nr, nc, r, c, dir), arr(r)(c) :: acc)
    }
  }

  val values = printSpiral(m)._2.reverse

  println(values)

  assert(values == List(1, 2, 3, 4, 5, 10, 15, 14, 13, 12, 11, 6, 7, 8, 9))
}
