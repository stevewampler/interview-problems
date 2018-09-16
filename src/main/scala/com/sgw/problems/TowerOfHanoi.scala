package com.sgw.problems

/**
  * Tower of Hanoi is a Mathematical Game.
  * There are 3 pegs (Rods), Source, Destination and Extra marked as S, D and E respectively,
  * and there are n discs, each of different size, which can be inserted into any of these three pegs.
  * All discs are initially inserted into Source peg in decreasing order (smallest at the top)
  * as shown in Picture 1.1 (for n=4).
  *
  *     |              |              |
  *    ---             |              |
  *   -----            |              |
  *  -------           |              |
  * ---------          |              |
  * -------------------------------------------
  *     S              D              E
  *
  * The problem is to move all of the disks from S to D. The final state should be:
  *
  *     |              |              |
  *     |             ---             |
  *     |            -----            |
  *     |           -------           |
  *     |          ---------          |
  * -------------------------------------------
  *     S              D              E
  *
  * There are 2 restrictions:
  * 1) Only one disc can be moved at a time.
  * 2) At any point in the process we should never place a larger disc on top of a smaller disc.
  *
  * From Meenakshi. Dynamic Programming for Coding Interviews:
  * A Bottom-Up approach to problem solving (p. 7). Notion Press. Kindle Edition.
  */
object TowerOfHanoi {
  /**
    * Iteratively solves the Tower of Hanoi problem.
    *
    * @param n number of disks
    * @param s the name of the source peg
    * @param d the name of the destination peg
    * @param e the name of the extra peg
    */
  private def go(n: Int, s: String = "s", d: String = "d", e: String = "e"): Unit = {

    if (n <= 0) return

    // solve the n-1 problem by swapping the destination and extra pegs
    go(n - 1, s = s, d = e, e = d)

    // move the nth disk to the current destination peg
    println(s"Move $n from $s to $d")

    // solve the n-1 problem again by swapping the source and extra pegs
    go(n - 1, s = e, d = d, e = s)
  }

  def main(args: Array[String]): Unit = {
    val n = if (args.length > 0) args(0).toInt else 4

    go(n)
  }
}
