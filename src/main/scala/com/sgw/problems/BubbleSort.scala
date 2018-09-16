package com.sgw.problems

object BubbleSort {

  /**
    * Sorts an array in place using a bubble sort.
    *
    * @param arr the array of item's of type T to sort
    * @param ordering the implicit ordering for items of type T
    * @tparam T the array's type
    *
    * @return the sorted array
    */
  def sort[T](arr: Array[T])(implicit ordering: Ordering[T]): Array[T] = {
    val n = arr.length

    (0 until (n - 1)).foldLeft(arr) { case (arr, i) =>
      ((i + 1) until n).foldLeft(arr) { case (arr, j) =>
        if (ordering.compare(arr(i), arr(j)) > 0) {
          swap(arr, i, j)
        } else {
          arr
        }
      }
    }
  }

  private def swap[T](arr: Array[T], i: Int, j: Int): Array[T] = {
    val tmp: T = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
    arr
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(5, 4, 2, 6, 9, 1, 4)

    sort(arr)

    assert(arr.sameElements(Array(1,2,4,4,5,6,9)))

    println(arr.mkString(","))
  }
}
