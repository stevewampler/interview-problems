package com.sgw.problems

import com.sgw.problems.GameOfLife.offsets

import scala.util.Random

/**
  * See The Model Thinker by Scott E. Page on page 172
  * @author swampler
  */
object LocalMajorityModel extends App {

  val n = 100
  val iterations = 200000
  val offsets = List(
    (-1, -1), (-1, 0), (-1, 1),
    ( 0, -1),          ( 0, 1),
    ( 1, -1), ( 1, 0), ( 1, 1)
  )

  val board: Array[Array[Boolean]] = Array.ofDim[Boolean](n, n)

  val requiredToFlip = 5 // must be between 0 and 8 inclusive

  val split = .5

  // random
  for (row <- 0 until n) {
    for (col <- 0 until n) {
      board(row)(col) = Random.nextDouble() > split
    }
  }

  // diag.
//  for (i <- 0 until n) {
//    if (i > 0) board(i - 1)(i) = true
//    board(i)(i) = true
//    if (i < (n - 1)) board(i + 1)(i) = true
//  }

  println(s"==========")
  printBoard(board)

  run(n, board)

  println(s"==========")
  printBoard(board)

  def vote(board: Array[Array[Boolean]]): (Int, Int) = {
    board.foldLeft((0, 0)) { case (acc, row) =>
      row.foldLeft(acc) { case ((numTrue, numFalse), value) =>
        if (value)
          (numTrue + 1, numFalse)
        else
          (numTrue, numFalse + 1)
      }
    }
  }

  def run(n: Int, board: Array[Array[Boolean]]): Array[Array[Boolean]] = {
    (0 until iterations).foreach { i =>
      iterate(n, board)
    }

    board
  }

  def iterate(n: Int, board: Array[Array[Boolean]], wrap: Boolean = true): Array[Array[Boolean]] = {
    val row = Random.nextInt(n)
    val col = Random.nextInt(n)

    val total = (0 until 8).map(offsets(_)).map { case (rowOffset, colOffset) =>
      if (wrap) {
        val offsetRow = if (row + rowOffset < 0) {
          n - 1
        } else if (row + rowOffset >= n) {
          0
        } else {
          row + rowOffset
        }

        val offsetCol = if (col + colOffset < 0) {
          n - 1
        } else if (col + colOffset >= n) {
          0
        } else {
          col + colOffset
        }

        if (board(offsetRow)(offsetCol)) 1 else 0
      } else {
        val offsetRow = row + rowOffset
        val offsetCol = col + colOffset

        if (offsetRow >= 0 && offsetRow < n && offsetCol >= 0 && offsetCol < n) {
          if (board(offsetRow)(offsetCol)) 1 else 0
        } else {
          0
        }
      }
    }.sum

    board(row)(col) = if (!board(row)(col) && total >= requiredToFlip)
      true
    else if (board(row)(col) && total <= 8 - requiredToFlip)
      false
    else
      board(row)(col)

    board
  }

  def printBoard(board: Array[Array[Boolean]]): Unit = {
    board.map { row =>
      row.map { cell =>
        if (cell) "*" else " "
      }.mkString("")
    }.foreach(println)

    val (numTrue, numFalse) = vote(board)

    val total = numTrue + numFalse
    val trueRatio = numTrue.toDouble / total.toDouble
    val falseRatio = 1.0 - trueRatio

    println(s"$trueRatio, $falseRatio")
  }
}
