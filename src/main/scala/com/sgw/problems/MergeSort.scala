package com.sgw.problems

object MergeSort {
  def sort(vals: Array[Int]): Array[Int] = if (vals.length <= 1) {
    vals
  } else {
    mergeSort(vals.take(vals.length / 2), vals.drop(vals.length / 2))
  }

  private def mergeSort(vals1: Array[Int], vals2: Array[Int]): Array[Int] =
    merge(sort(vals1), sort(vals2))

  private def merge(vals1: Array[Int], vals2: Array[Int]): Array[Int] = {
    if (vals1.isEmpty) {
      return vals2
    }

    if (vals2.isEmpty) {
      return vals1
    }

    var i1 = 0
    var i2 = 0

    val result = new Array[Int](vals1.length + vals2.length)

    result.indices.foldLeft(result) { case (acc, i) =>
      val v = if (i1 < vals1.length) {
        val v1 = vals1(i1)

        if (i2 < vals2.length) {
          val v2 = vals2(i2)

          if (v2 <= v1) {
            i2 = i2 + 1
            v2
          } else {
            i1 = i1 + 1
            v1
          }
        } else {
          i1 = i1 + 1
          v1
        }
      } else {
        if (i2 < vals2.length) {
          val v2 = vals2(i2)
          i2 = i2 + 1
          v2
        } else {
          throw new RuntimeException("This shouldn't happen.")
        }
      }

      acc(i) = v

      acc
    }
  }

  // conceptually a bit better, but dropWhile and takeWhile are expensive
  def merge2(vals1: Array[Int], vals2: Array[Int]): Array[Int] = {
    val (remainder, result) = vals1.foldLeft((vals2, new Array[Int](0))) { case ((vals2, acc), v1) =>
      (
        vals2.dropWhile { v2 =>
          v2 < v1
        },
        acc ++ vals2.takeWhile { v2 =>
          v2 < v1
        } :+ v1
      )
    }

    result ++ remainder
  }

  def merge3(vals1: Array[Int], vals2: Array[Int]): Array[Int] = {
    var (newI2, newI, result) = vals1.foldLeft((0, 0, new Array[Int](vals1.length + vals2.length))) { case ((i2, i, acc), v1) =>
      var newI2 = i2
      var newI = i

      while (newI2 < vals2.length && vals2(newI2) < v1) {
        acc(newI) = vals2(newI2)
        newI2 = newI2 + 1
        newI = newI + 1
      }

      acc(newI) = v1
      newI = newI + 1

      (newI2, newI, acc)
    }

    while (newI2 < vals2.length) {
      result(newI) = vals2(newI2)
      newI2 = newI2 + 1
      newI = newI + 1
    }

    result
  }
}
