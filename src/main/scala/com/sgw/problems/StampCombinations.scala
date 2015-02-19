package com.sgw.problems

/**
 * MakeChange using stamps, but the stamp values are not divisible into the amount.
 *
 * Has to be solved brute force by finding all of the stamp combinations that add up to the specified amount,
 * and then return the minimum number of stamps.
 */
object StampCombinations {
  def makeChange(value: Int, stamps: List[Int], amount: Int, level: Int, stampValues: List[Int]): List[Int] = {
    var i = level

    while (i < stampValues.size) {
      val newValue = value + stampValues(i)
      val newStamps = stampValues(i) :: stamps

      if (newValue == amount) {
        return newStamps
      }

      if (newValue > amount) {
        return Nil
      }

      val foundStamps = makeChange(newValue, newStamps, amount, i + 1, stampValues)

      if (foundStamps.nonEmpty) {
        return foundStamps
      }

      i = i + 1
    }

    Nil
  }

  def main (args: Array[String]) {
    println(makeChange(0, Nil, 82, 0, List(42, 41, 1, 41)))
    println(makeChange(0, Nil, 83, 0, List(42, 41, 1, 41)))
    println(makeChange(0, Nil, 84, 0, List(42, 41, 1, 41)))
    println(makeChange(0, Nil, 85, 0, List(42, 41, 1, 41)))
  }
}
