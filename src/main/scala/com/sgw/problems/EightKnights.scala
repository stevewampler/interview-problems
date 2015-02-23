package com.sgw.problems

import scala.annotation.tailrec
import scala.util.Try

/**
 * Given n knights on an n x n chess board, print out of the possible positions of all n knights such that no two
 * knights can check each other.
 */
object EightKnights {
  private val OFFSETS = List(
    ( 0,  0),
    (-1, -2),
    (-2, -1),
    (-2,  1),
    (-1,  2),
    ( 1,  2),
    ( 2,  1),
    ( 2, -1),
    ( 1, -2)
  )

  case class Knight(id: Int, n: Int, row: Int, col: Int) {
    def advance(): Option[Knight] = {
      if (row + 1 >= n) {
        if (col + 1 >= n) {
          None
        } else {
          Some(copy(row = 0, col = col + 1))
        }
      } else {
        Some(copy(row = row + 1))
      }
    }
  }

  private def go(n: Int, knight: Knight, solution: List[Knight], solutions: List[List[Knight]]): List[List[Knight]] = {
    // if the knight is checked ...
    if (inCheck(knight, solution)) {
      // move the knight if possible, otherwise return the current solutions
      knight.advance().map(k => go(n, k, solution, solutions)).getOrElse(solutions)
    } else {
      // a potential (and perhaps partial) solution
      val newSolution = knight :: solution

      // if we've found a complete solution ...
      if (newSolution.size == n) {
        // add it to the current solutions
        val newSolutions = newSolution :: solutions

        // advance the knight if possible, otherwise return the current solutions
        knight.advance().map(k => go(n, k, solution, newSolutions)).getOrElse(newSolutions)
      } else {
        // create a new knight at the same position as the current knight
        val newKnight = knight.copy(knight.id + 1)

        // advance the new knight if possible; otherwise return the current solutions
        val newSolutions = newKnight.advance().map(k => go(n, k, newSolution, solutions)).getOrElse(solutions)

        // advance the current knight if possible looking for additional solutions; otherwise return the new solutions
        knight.advance().map(k => go(n, k, solution, newSolutions)).getOrElse(newSolutions)
      }
    }
  }

  private def inCheck(knight: Knight, solution: List[Knight]): Boolean = OFFSETS.map {
    case (rowOffset, colOffset) => (knight.row + rowOffset, knight.col + colOffset)
  }.exists {
    case (checkRow, checkCol) => solution.exists(otherKnight => otherKnight.row == checkRow && otherKnight.col == checkCol)
  }

  def solve(n: Int): List[List[Knight]] = {
    go(n, Knight(1, n, 0, 0), Nil, Nil)
  }

  def drawBoard(solution: List[Knight]): String = {
    val board: Array[Array[Int]] = Array.ofDim(solution.size, solution.size)

    solution.foreach(knight => board(knight.row)(knight.col) = knight.id)

    board.map(row => row.map(b => if (b == 0) " " else b.toString).mkString("|", "|", "|")).mkString("\n")
  }

  def main (args: Array[String]) {
    val n = if (args.size == 0) 3 else args(0).toInt

    val solutions = solve(n)

    solutions.reverse.zipWithIndex.foreach {
      case (solution, index) => {
        println(s"Solution $index:")
        println(drawBoard(solution.reverse))
      }
    }
  }
}
