package com.sgw.problems

import scala.annotation.tailrec

/**
  * Print out the values of a matrix in spiral order.
  */
object SpiralMatrix extends App {
  val matrix: Array[Array[Int]] = Array(
    Array( 1,  2,  3,  4),
    Array( 5,  6,  7,  8),
    Array( 9, 10, 11, 12),
    Array(13, 14, 15, 16)
  )

  printSpiralMatrix(matrix, 0, 0, 0, matrix(0).size, 0, matrix.size, 1, 0)

  @tailrec
  def printSpiralMatrix(
    matrix: Array[Array[Int]],
    col: Int, row: Int,
    colMin: Int, colMax: Int,
    rowMin: Int, rowMax: Int,
    colDir: Int, rowDir: Int
  ): Unit = {
    if (colMin == colMax || rowMin == rowMax) return

    println(matrix(row)(col))

    val nextCol = col + colDir
    val nextRow = row + rowDir

    // each time we hit the edge of a range, change directions and shrink the range; otherwise, keep going in the same direction (right, down, left, or up)
    val (newCol, newRow, newColMin, newColMax, newRowMin, newRowMax, newColDir, newRowDir) =
      if (nextCol >= colMax) {
        (col, row + 1, colMin, colMax, rowMin + 1, rowMax, 0, 1)
      } else if (nextRow >= rowMax) {
        (col - 1, row, colMin, colMax - 1, rowMin, rowMax, -1, 0)
      } else if (nextCol < colMin) {
        (col, row - 1, colMin, colMax, rowMin, rowMax - 1, 0, -1)
      } else if (nextRow < rowMin) {
        (col + 1, row, colMin + 1, colMax, rowMin, rowMax, 1, 0)
      } else {
        (nextCol, nextRow, colMin, colMax, rowMin, rowMax, colDir, rowDir)
      }

    printSpiralMatrix(matrix, newCol, newRow, newColMin, newColMax, newRowMin, newRowMax, newColDir, newRowDir)
  }
}
