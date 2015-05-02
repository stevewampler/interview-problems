package com.sgw.problems

/**
 * Given a wall of bricks of various heights, calculate how much water the wall can hold.
 *
 * For example, this wall:
 *
 *        X
 *    X   X X
 *   _X_XXXXXX
 *
 *   020113121 <- brick heights
 *
 * Can hold 5 units of water.
 *
 * Approach:
 *
 * Find the column of the wall that has the maximum height.
 * Work from left to right up to the overall max column keeping track of the current max column height as you go and the
 * amount of water. For each column, subtract the column's height from the current max column height (keep only positive
 * amounts). That's how much water that column can hold. Add it to the total.
 * Now work the problem from the right to the left up to the overall max column.
 * Add the two amounts to get the final answer.
 */
object WaterWall {
  private def canHold(maxHeight: Int, amount: Int, height: Int): (Int, Int) =
    (maxHeight.max(height), amount + 0.max(maxHeight - height))

  private def canHoldImpl(wall: Seq[Int]): Int = wall.foldLeft((0, 0)) {
    case ((maxHeight, amount), height) => canHold(maxHeight, amount, height)
  }._2

  def canHold(wall: Seq[Int]): Int = {
    val (_, maxIndex) = wall.zipWithIndex.maxBy {
      case (height, index) => height
    }

    val fromLeftAmount = canHoldImpl(wall.take(maxIndex))

    val fromRightAmount = canHoldImpl(wall.reverse.take(wall.length - maxIndex - 1))

    fromLeftAmount + fromRightAmount
  }
}
