package com.sgw.hackerrank.greedyalgos

import java.io.{BufferedReader, InputStream, InputStreamReader}

/**
  * Given an array of integers, find the minimum absolute difference between any two values in the array.
  *
  * Strategy:
  *
  * Sort the array and then loop over it from 0 until n-1 looking for the min of (a[i] - a[i+1]).abs.
  *
  * From https://www.hackerrank.com/challenges/minimum-absolute-difference-in-an-array/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=greedy-algorithms
  */
object MinAbsDifference {

  // brute force O(n^2)
//  def minimumAbsoluteDifference(arr: Array[Int]): Int = {
//    val n = arr.length
//
//    (0 until n).flatMap { i =>
//      (i + 1 until n).map { j =>
//        // println((i, j))
//        (arr(i) - arr(j)).abs
//      }
//    }.min
//  }

  // sorted ... but overkill
//  def minimumAbsoluteDifference(arr: Array[Int]): Int = {
//    val n = arr.length
//
//    val sortedArr = arr.sorted
//
//    var min: Int = Int.MaxValue
//
//    (0 until n).foreach { i =>
//      val v1 = sortedArr(i)
//      var j = i + 1
//      var done = false
//
//      while (j < n && !done) {
//        val v2 = sortedArr(j)
//
//        val v = (v1 - v2).abs
//
//        if (v < min) {
//          min = v
//        } else {
//          done = true
//        }
//
//        j = j + 1
//      }
//    }
//
//    min
//  }

  // simplified O(n log(n))
  def minimumAbsoluteDifference(arr: Array[Int]): Int = {
    val n = arr.length

    // O(n log(n))
    val sortedArr = arr.sorted

    // O(n)
    (0 until n - 1).foldLeft(Int.MaxValue) { case (min, i) =>
      min.min((sortedArr(i) - sortedArr(i + 1)).abs)
    }
  }

  def run(inputStream: InputStream): Long = {
    val reader = new BufferedReader(new InputStreamReader(inputStream))

    val n = reader.readLine.trim.toInt

    val arr = reader.readLine.split(" ").map(_.trim.toInt)
    val result = minimumAbsoluteDifference(arr)

    result
  }
}
