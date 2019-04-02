package com.sgw.hackerrank.sorting

/**
  * From https://www.hackerrank.com/challenges/ctci-bubble-sort/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=sorting
  */
object BubbleSortCountSwaps {

  // bubble sort and count the swaps
  def countSwaps(a: Array[Int]): (List[Int], Int) = {
    var swaps = 0

    val n = a.length

    (0 until n).foreach { i =>
      (0 until n - 1).foreach { j =>
        // Swap adjacent elements if they are in decreasing order
        if (a(j) > a(j + 1)) {
          val tmp = a(j)
          a(j) = a(j + 1)
          a(j + 1) = tmp
          swaps = swaps + 1
        }
      }
    }

    (a.toList, swaps)
  }

  def main(args: Array[String]) {
    {
      val result = countSwaps(Array(1, 2, 3))
      println(result)
      assert(result == (List(1, 2, 3), 0))
    }

    {
      val result = countSwaps(Array(3, 2, 1))
      println(result)
      assert(result == (List(1, 2, 3), 3))
    }

    {
      val result = countSwaps(Array(1, 4, 3, 2))
      println(result)
      assert(result == (List(1, 2, 3, 4), 3))
    }
  }
}

