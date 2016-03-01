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

    result.indices.foldLeft(result) {
      case (acc, i) => {
        if (i1 < vals1.length) {
          val v1 = vals1(i1)

          if (i2 < vals2.length) {
            val v2 = vals2(i2)

            if (v2 <= v1) {
              acc(i) = v2
              i2 = i2 + 1
            } else {
              acc(i) = v1
              i1 = i1 + 1
            }
          } else {
            acc(i) = v1
            i1 = i1 + 1
          }
        } else {
          if (i2 < vals2.length) {
            val v2 = vals2(i2)
            acc(i) = v2
            i2 = i2 + 1
          } else {
            throw new RuntimeException("This shouldn't happen.")
          }
        }

        acc
      }
    }
  }
}
