package com.sgw.problems

/**
 * Given n queens on an n x n chess board, print out all of the possible positions of all n queues such that no two
 * queens can check each other.
 */
object EightQueens extends App {
  private def solve(n: Int, queen: (Int, Int) = (0, 0), solution: List[(Int, Int)] = List(), solutions: List[List[(Int, Int)]] = List()): List[List[(Int, Int)]] = {
    val newSolutions = queen match {
      case (queenCol, _) if queenCol >= n => return solution :: solutions // we're off the right side of the board, so we found a new solution, add it to the list
      case (_, queenRow) if queenRow >= n => return solutions // we're off the bottom of the board and haven't found a solution, so this branch is a dead end
      case (queenCol, _) if isNotInCheck(queen, solution) => solve(n, (queenCol + 1, 0), queen :: solution, solutions) // the queen is not in check, so add the queen to the current solution and iterate to add another queen in the next column at the top of the board
      case _ => solutions // queen is in check, so this branch is not a solution
    }

    // move the current (in check) queen to the next row and try again
    solve(n, (queen._1, queen._2 + 1), solution, newSolutions)
  }

  private def isNotInCheck(queen1: (Int, Int), solution: List[(Int, Int)]): Boolean =
    solution.forall(queen2 => isNotInCheck(queen1)(queen2))

  private def isNotInCheck(queen1: (Int, Int))(queen2: (Int, Int)): Boolean = !isInCheck(queen1)(queen2)

  private def isInCheck(queen1: (Int, Int))(queen2: (Int, Int)): Boolean =
      (queen1._2 == queen2._2) || // are the queens on the same row? check, or
      ((queen2._1 - queen1._1).abs == (queen2._2 - queen1._2).abs) // are the queens on the same diagonal? check

  private def drawBoard(solution: List[(Int, Int)]): String = {
    val builder = new StringBuilder()

    val n = solution.size

    solution.map {
      case (row, col) => {
        builder.append("+-" * n).append("+\n")
        builder.append("| " * col).append("|Q").append("| " * (n - col - 1)).append("|\n")
      }
    }

    builder.append("+-" * solution.size).append("+\n")

    builder.toString()
  }

  private val n = if (args.nonEmpty) args(0).toInt else 8

  private val solutions = solve(n = n)

  solutions.zipWithIndex.foreach{
    case (solution, index) => {
      println(s"Solution $index:")
      println(drawBoard(solution.reverse))
      // solution.reverse.foreach(println)
    }
  }
}
