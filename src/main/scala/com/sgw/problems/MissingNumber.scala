package com.sgw.problems

/**
  * Find the missing number in an array of integers that are ordered randomly.
  */
object MissingNumber extends App {
  val array = Array[Int](5,4,1,2,0)

  // brute force O(n log n)
  val missingValue = array.sorted.zipWithIndex.dropWhile {
    case (i, index) => i == index
  }.headOption.map {
    case (i, index) => index
  }.getOrElse(-1)

  println(missingValue)

  // using math O(n)
  val n = array.size
  val total = n * (n + 1) / 2
  println(total)
  val missingValue2 = total - array.sum
  println(missingValue2)

  // using foldLeft
  val indexSum = (1 to n).sum // one based and inclusive on both ends of the range: [1,n]
  val arraySum = array.sum

  println(indexSum)
  println(arraySum)
  println(indexSum - arraySum)
}
