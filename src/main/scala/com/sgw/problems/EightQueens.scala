package com.sgw.problems

/**
 * Given n queens on an n x n chess board, print out of the possible positions of all n queues such that no two
 * queens can check each other.
 */
object EightQueens {
  private def go(n: Int, q: (Int, Int), solution: List[(Int, Int)], solutions: List[List[(Int, Int)]]): List[List[(Int, Int)]] = {
    val newSolutions = q match {
      case (qx, _) if qx >= n => return solution :: solutions // found a new solution, add it to the list
      case (_, qy) if qy >= n => return solutions // dead end
      case (qx, _) if isNotInCheck(q, solution) => go(n, (qx + 1, 0), q :: solution, solutions) // not in check, so add it
      case _ => solutions // queen is in check, so this branch is not a solution
    }

    // go to the next row
    go(n, (q._1, q._2 + 1), solution, newSolutions)
  }

  private def isNotInCheck(q: (Int, Int), solution: List[(Int, Int)]): Boolean =
    solution.forall(isNotInCheck(q))

  private def isNotInCheck(q1: (Int, Int))(q2: (Int, Int)): Boolean = !isInCheck(q1)(q2)

  private def isInCheck(q1: (Int, Int))(q2: (Int, Int)): Boolean =
    (q1._1 == q2._1) || (q1._2 == q2._2) || ((q2._1 - q1._1).abs == (q2._2 - q1._2).abs)

  def solve(n: Int): List[List[(Int, Int)]] = go(n, (0, 0), List(), List())

  def drawBoard(solution: List[(Int, Int)]): String = {
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

  def main(args: Array[String]): Unit = {
    val solutions = solve(if (args.size > 0) args(0).toInt else 8)

    solutions.zipWithIndex.foreach{
      case (solution, index) => {
        println(s"Solution $index:")
        println(drawBoard(solution.reverse))
        // solution.reverse.foreach(println)
      }
    }
  }
}
