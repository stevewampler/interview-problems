package com.sgw.problems

/**
  * Given a 2D matrix of booleans, return the number of squares within the array whose upper left corner contains
  * a value of true.
  *
  * For example, the following 3 x 6 matrix of all true values:
  *
  * [ true, true, true, true, true, true ]
  * [ true, true, true, true, true, true ]
  * [ true, true, true, true, true, true ]
  *
  * contains 32 squares all of which have a true value in the upper left corner, so the result will be 32.
  */
object UpperLeftSquares {
  def solve(matrix: Array[Array[Boolean]]): Int = {
    val h = matrix.length

    if (h == 0) return 0

    val w = matrix(0).length

    val d = w.min(h)

    def count(r: Int, c: Int): Int = d.min(w - c).min(h - r)

    (0 until h).foldLeft(0) { case (acc, r) =>
      (0 until w).foldLeft(acc) { (acc, c) =>
        if (matrix(r)(c)) {
          acc + count(r, c)
        } else {
          acc
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    assert(
      solve(
        Array()
      ) == 0
    )

    assert(
      solve(
        Array(
          Array(true)
        )
      ) == 1
    )

    assert(
      solve(
        Array(
          Array(true, true, true, true, true, true),
          Array(true, true, true, true, true, true),
          Array(true, true, true, true, true, true)
        )
      ) == 32
    )

    assert(
      solve(
        Array(
          Array(false, false, false, false, false, false),
          Array(false, false, false, false, false, false),
          Array(false, false, false, false, false, false)
        )
      ) == 0
    )

    assert(
      solve(
        Array(
          Array(true, false, false, false, false, false),
          Array(false, false, false, false, false, false),
          Array(false, false, false, false, false, false)
        )
      ) == 3
    )
  }
}
