package com.sgw.projecteuler

/**
 * From: https://projecteuler.net/problem=6
 *
 * The sum of the squares of the first ten natural numbers is 385.
 *
 * The square of the sum of the first ten natural numbers is,
 *
 *  (1 + 2 + ... + 10)2 = 552 = 3025
 *
 * Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
 *
 * Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
 */
object P6SumSquareDifference {
  def sumOfSquares(n: Int): Int = n * (2 * n + 1) * (n + 1) / 6
  def sumOf1ToN(n: Int): Int = n * (n + 1) / 2

  def functionalDifference(n: Int): (Int, Int, Int) = {
    val sumOfSqs = sumOfSquares(n)
    val sum = sumOf1ToN(n)
    val squareOfSum = sum * sum

    (sumOfSqs, squareOfSum, squareOfSum - sumOfSqs)
  }

  def bruteForceDifference(maxNum: Int): (Int, Int, Int) = {
    val sumOfSquares = (1 to maxNum).map(i => i * i).sum
    val sum = (1 to maxNum).sum
    val squareOfSum  = sum * sum

    (sumOfSquares, squareOfSum, squareOfSum - sumOfSquares)
  }

  def main(args: Array[String]) {
    val (sumOfSqs, squareOfSum, diff) = bruteForceDifference(100)

    println(sumOfSqs)
    println(squareOfSum)
    println(diff)

    val (sumOfSqs2, squareOfSum2, diff2) = functionalDifference(100)

    println(sumOfSqs2)
    println(squareOfSum2)
    println(diff2)
  }
}
