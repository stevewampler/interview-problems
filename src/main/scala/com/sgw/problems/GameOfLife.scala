package com.sgw.problems

import scala.util.Random

/**
  * See The Model Thinker by Scott E. Page on page 176
  * @author swampler
  */
object GameOfLife extends App {

  val n = 10
  val iterations = 20
  val offsets = List(
    (-1, -1), (-1, 0), (-1, 1),
    ( 0, -1),          ( 0, 1),
    ( 1, -1), ( 1, 0), ( 1, 1)
  )

  var board: Array[Array[Boolean]] = Array.ofDim[Boolean](n, n)

  // random
//  for (row <- 0 until n) {
//    for (col <- 0 until n) {
//      board(row)(col) = Random.nextBoolean()
//    }
//  }

  // blinker: blinks between horizontal and vertical line
//  board(1)(2) = true
//  board(1)(3) = true
//  board(1)(4) = true

  // glider: glides down and to the right
//  board(1)(2) = true
//  board(2)(3) = true
//  board(3)(1) = true
//  board(3)(2) = true
//  board(3)(3) = true

  // r-pentomino: creates a complex seq. of patterns, generated cliders and blinkers
  board(1)(2) = true
  board(1)(3) = true
  board(2)(1) = true
  board(2)(2) = true
  board(3)(2) = true

  println(s"0 ==========")
  printBoard(n, board)

  (1 to iterations).foreach { i =>
    board = iterate(n, board)
    println(s"$i ==========")
    printBoard(n, board)
  }

  def iterate(n: Int, board: Array[Array[Boolean]]): Array[Array[Boolean]] = {
    val newBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](n, n)

    for (row <- 0 until n) {
      for (col <- 0 until n) {
        val total = (0 until 8).map(offsets(_)).map { case (rowOffset, colOffset) =>
          val offsetRow = row + rowOffset
          val offestCol = col + colOffset
          if (offsetRow >= 0 && offsetRow < n && offestCol >= 0 && offestCol < n) {
            if (board(offsetRow)(offestCol)) 1 else 0
          } else {
            0
          }
        }.sum

        newBoard(row)(col) = !board(row)(col) && total == 3 || (board(row)(col) && !(total < 2 || total > 3))
      }
    }

    newBoard
  }

  def printBoard(n: Int, board: Array[Array[Boolean]]): Unit = {
    board.map { row =>
      row.map { cell =>
        if (cell) "*" else "_"
      }.mkString("")
    }.foreach(println)
  }
}
