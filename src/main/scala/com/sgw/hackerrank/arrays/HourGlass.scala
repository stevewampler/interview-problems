/**
  * From https://www.hackerrank.com/challenges/2d-array/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=arrays
  *
  * An hour-glass is a 3x3 "I" shaped thing in an array. A 6x6 array has 16 of them. Return the max sum over all the hour glasses in
  * a given 2D array.
  */
object HourGlass {

  // Complete the hourglassSum function below.
  def hourglassSum(arr: Array[Array[Int]]): Int = {
    def go(r: Int, c: Int): Int = {
      Seq(
        0 -> 0,
        0 -> 1,
        0 -> 2,
        1 -> 1,
        2 -> 0,
        2 -> 1,
        2 -> 2
      ).map { case (dr, dc) =>
        arr(r + dr)(c + dc)
      }.sum
    }

    val nr = arr.size
    val nc = arr(0).size

    (0 until nr - 2).flatMap { r =>
      (0 until nc - 2).map { c =>
        go(r, c)
      }
    }.max
  }

  def main(args: Array[String]) {
    val arr = Array(
      Array(1, 1, 1, 0, 0, 0),
      Array(0, 1, 0, 0, 0, 0),
      Array(1, 1, 1, 0, 0, 0),
      Array(0, 0, 2, 4, 4, 0),
      Array(0, 0, 0, 2, 0, 0),
      Array(0, 0, 1, 2, 4, 0)
    )

    assert(hourglassSum(arr) == 19)
  }
}
