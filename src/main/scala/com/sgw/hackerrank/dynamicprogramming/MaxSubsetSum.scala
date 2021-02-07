package com.sgw.hackerrank.dynamicprogramming

import java.io.{BufferedReader, InputStream, InputStreamReader}

import com.sgw.utils.Timer

import scala.util.Try

/**
  * Given an array of integers, find the subset of non-adjacent elements with the maximum sum. Calculate the sum of that subset.
  *
  * For example, given an array [-2, 1, 3, -4, 5] we have the following possible subsets and sums:
  *
  * Subset      Sum
  * [-2]        -2
  * [-2, 3]      1
  * [-2, 3, 5]   6
  * [-2, -4]    -6
  * [-2, 5]      3
  * [1]          1
  * [1, -4]     -3
  * [1, 5]       6
  * [3]          3
  * [3, 5]       8
  * [-4]        -4
  * [5]          5
  *
  * Our maximum subset sum is 8.
  *
  * From https://www.hackerrank.com/challenges/max-array-sum/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=dynamic-programming
  */
object MaxSubsetSum {
  // brute force recursive: O(n^2)
  private def maxSubsetSumBruteForce(arr: Array[Int]): Int = {
    def go2(arr1: Array[Int]): Int = {
      def go(arr2: Array[Int]): Int = {
        if (arr2.isEmpty) return 0
        if (arr2.size == 1) return arr2(0)
        if (arr2.size == 2) return arr2.max

        arr2(0) + go2(arr2.slice(2, arr2.size))
      }

      arr1.indices.map { i =>
        go(arr1.slice(i, arr1.size))
      }.max
    }

    go2(arr)
  }

  /**
    * This method takes the solutions for the i+1 and i+2 and uses those to calculate the solution for i.
    *
    * @param i the index into arr
    * @param arr an array of integers
    * @param currentMax the running maximum of all the previously calculated maximums
    * @param iPlus1Max the maximum value calculated for entry i+1
    * @param iPlus2Max the maximum value calculated for entry i+2
    * @return a three-tuple of max values; the running max value including the max value calculated
    *         for index i, the max value calculated for index i, and the max of the previous max values for i+1 and i+2
    */
  private def maxFor(i: Int, arr: Array[Int], currentMax: Int, iPlus1Max: Int, iPlus2Max: Int): (Int, Int, Int) = {
    val newMax = (arr(i) + iPlus2Max).max(arr(i))

    val newCurrentMax = newMax.max(currentMax)

    val newIPlus1Max = newMax
    val newIPlus2Max = iPlus1Max.max(iPlus2Max)

    (newCurrentMax, newIPlus1Max, newIPlus2Max)
  }

  // O(n)
  def maxSubsetSum(arr: Array[Int]): Int = {
    val n = arr.length

    if (n == 0) return 0
    if (n == 1) return arr(0)
    if (n == 2) return arr.max

    // fold over the array in reverse order to accumulate the current overall max, the max at i+1, and the max at i+2
    (n - 3 to 0 by -1).foldLeft((Int.MinValue, arr(n - 2), arr(n - 1))) { case ((currentMax, iPlus1Max, iPlus2Max), i) =>
      maxFor(i, arr, currentMax, iPlus1Max, iPlus2Max)
    }._1
  }

  def run(inputStream: InputStream): Try[Int] = {
    val reader = new BufferedReader(new InputStreamReader(inputStream))

    reader.readLine.trim.toInt

    val arr = reader.readLine.split(" ").map(_.trim.toInt)

    val n = arr.length

    val maybeBruteForceResult: Option[Int] = if (n <= 10) {
      Some(maxSubsetSumBruteForce(arr))
    } else {
      None
    }

    Timer {
      maxSubsetSum(arr)
    }.map { case (time, result) =>
      println(s"${arr.slice(0, arr.length.min(15)).mkString(",")}")
      println(s"$time, $result, $maybeBruteForceResult")

      result
    }
  }
}
