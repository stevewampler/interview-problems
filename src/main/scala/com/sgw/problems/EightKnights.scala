package com.sgw.problems

import scala.annotation.tailrec
import scala.util.Try

/**
 * Given n knights on an n x n chess board, print out of the possible positions of all n knights such that no two
 * knights can check each other.
 */
object EightKnights {
  // the attach positions of a knight relative to the knight's current position on the board
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
    // advances the knight one position on the board by row and then column
    def advance(): Knight = {
      if (row + 1 >= n) {
        copy(row = 0, col = col + 1)
      } else {
        copy(row = row + 1)
      }
    }

    // checks to see if this knight's current position is off the right edge of the board
    def isOffBoard: Boolean = col >= n
  }

  @tailrec
  private def go(n: Int, knight: Knight, solution: List[Knight], solutions: List[List[Knight]]): List[List[Knight]] = {
    // we stop the recursion whenever a knight is advanced off of the right side of the board
    if (knight.isOffBoard) return solutions

    // get the current list of solutions, advance the knight, and recurse to find for more solutions
    go(n, knight.advance(), solution, getSolutions(n, knight, solution, solutions))
  }

  private def getSolutions(n: Int, knight: Knight, solution: List[Knight], solutions: List[List[Knight]]): List[List[Knight]] =
    // if the knight is checked ..
    if (inCheck(knight, solution)) {
      // nothing new to add, so just return the current list of solutions
      solutions
    } else { // knight is not checked, so ...
      // add the knight the solution
      val newSolution = knight :: solution

      // if we've found a complete solution ...
      if (newSolution.size == n) {
        // add the solution to the list of solutions
        newSolution :: solutions
      } else {
        // otherwise; we don't have a complete solution, so
        // create a new knight at the same position as the current knight and ...
        val newKnight = knight.copy(knight.id + 1)

        // advance the new knight and recurse to find for more solutions
        go(n, newKnight.advance(), newSolution, solutions)
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
