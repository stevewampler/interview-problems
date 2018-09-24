package com.sgw.problems

import com.sgw.problems.TrainTickets.n

/**
  * Given a matrix of order N*N. What are the total number of ways in which we can move from the top-left cell
  * (arr[0][0] ) to the bottom-right cell (arr[N-1][N-1]), given that we can only move either downward or rightward?
  *
  * Meenakshi. Dynamic Programming for Coding Interviews: A Bottom-Up approach to problem solving (p. 40). Notion Press. Kindle Edition.
  */
object TraverseMatrix extends App {

  // brute force (sub-paths are calculated multiple times)
  def countPaths1(n: Int, row: Int = 0, col: Int = 0, callCount: Int = 1): (Int, Int) = {
    if (row == n - 1 && col == n - 1) {
      (0, callCount) // from the lower right corner there are no additional paths
    } else if (row == n - 1) {
      (1, callCount) // from bottom edge there is only one path
    } else if (col == n - 1) {
      (1, callCount) // from the right edge there is only one path
    } else {
      // from all other locations there are two paths
      val (count1, newCallCount1) = countPaths1(n, row + 1, col, callCount + 1)
      val (count2, newCallCount2) = countPaths1(n, row, col + 1, callCount + 1)

      (count1 + count2, newCallCount1 + newCallCount2)
    }
  }

  (1 to 10).foreach { n =>
    val (count, callCount) = countPaths1(n)
    println(s"n=$n, num paths=$count, call count = $callCount")
  }

  // memorized version
  def countPaths2(n: Int): (Int, Int) = {
    def go(n: Int, row: Int = 0, col: Int = 0, memo: Array[Array[Int]], callCount: Int = 1): (Int, Int) = {
      if (memo(row)(col) != Int.MaxValue) {
        (memo(row)(col), callCount)
      } else {
        if (row == n - 1 && col == n - 1) {
          (0, callCount) // from the lower right corner there are no additional paths
        } else if (row == n - 1) {
          (1, callCount) // from bottom edge there is only one path
        } else if (col == n - 1) {
          (1, callCount) // from the right edge there is only one path
        } else {
          // from all other locations there are two paths
          val (count1, newCallCount1) = go(n, row + 1, col, memo, callCount + 1)
          val (count2, newCallCount2) = go(n, row, col + 1, memo, callCount + 1)

          memo(row)(col) = count1 + count2

          (count1 + count2, newCallCount1 + newCallCount2)
        }
      }
    }

    val memo: Array[Array[Int]] = Array.ofDim(n, n)

    (0 until n).foreach { i =>
      (0 until n).foreach { j =>
        memo(i)(j) = Int.MaxValue
      }
    }

    go(n, row = 0, col = 0, memo)
  }

  (1 to 10).foreach { n =>
    val (count, callCount) = countPaths2(n)
    println(s"n=$n, num paths=$count, call count = $callCount")
  }
}
