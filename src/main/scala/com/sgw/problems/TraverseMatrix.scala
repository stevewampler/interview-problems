package com.sgw.problems

/**
  * Given a matrix of order N*N. What are the total number of ways in which we can move from the top-left cell
  * (arr[0][0] ) to the bottom-right cell (arr[N-1][N-1]), given that we can only move either downward or rightward?
  *
  * Meenakshi. Dynamic Programming for Coding Interviews: A Bottom-Up approach to problem solving (p. 40). Notion Press. Kindle Edition.
  */
object TraverseMatrix extends App {

  def countPaths(n: Int, row: Int = 0, col: Int = 0): Int = {
    if (row == n - 1 && col == n - 1) {
      0 // from the lower right corner there are no additional paths
    } else if (row == n - 1) {
      1 // from bottom edge there is only one path
    } else if (col == n - 1) {
      1 // from the right edge there is only one path
    } else {
      // from all other locations there are two paths
      countPaths(n, row + 1, col) + countPaths(n, row, col + 1)
    }
  }

  (1 to 5).foreach { n =>
    println(s"n=$n paths=${countPaths(n)}")
  }
}
