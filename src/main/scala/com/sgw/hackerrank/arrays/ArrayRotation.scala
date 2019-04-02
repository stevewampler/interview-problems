package com.sgw.hackerrank.arrays

/**
  * From https://www.hackerrank.com/challenges/ctci-array-left-rotation/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=arrays&h_r=next-challenge&h_v=zen
  *
  * Write a function to rotate the values in an array "left" or "right" by a specified number of positions.
  */
object ArrayRotation {

  // Complete the rotLeft function below.
  def rotLeft(a: Array[Int], d: Int): Array[Int] = {
    a.drop(d) ++ a.take(d)
  }

  def rotRight(a: Array[Int], d: Int): Array[Int] = {
    rotLeft(a, a.length - d)
  }

  def main(args: Array[String]) {
    println(rotLeft(Array(1, 2, 3, 4, 5, 6), 4).mkString(","))
    assert(rotLeft(Array(1, 2, 3, 4, 5, 6), 4).mkString(",") == Array(5, 6, 1, 2, 3, 4).mkString(","))

    println(rotRight(Array(1, 2, 3, 4, 5, 6), 4).mkString(","))
    assert(rotRight(Array(1, 2, 3, 4, 5, 6), 4).mkString(",") == Array(3, 4, 5, 6, 1, 2).mkString(","))
  }
}

