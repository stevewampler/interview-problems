package com.sgw.problems

import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

/**
  * John works at a clothing store.
  * He has a large pile of socks that he must pair by color for sale. Given an array of integers
  * representing the color of each sock, determine how many pairs of socks with matching colors there are.
  *
  * For example, there are n=7 socks with colors ar=[1,2,1,2,1,3,2].
  * There is one pair of color 1 and one of color 2.
  * There are three odd socks left, one of each color.
  * The number of pairs is 2.
  *
  * Function Description
  *
  * Complete the sockMerchant function. It must return an integer representing the number of matching pairs of socks that are available.
  *
  * sockMerchant has the following parameter(s):
  *
  * n: the number of socks in the pile
  * ar: the colors of each sock
  *
  * Input Format
  *
  * The first line contains an integer n, the number of socks represented in ar.
  * The second line contains n space-separated integers describing the colors ar[i] of the socks in the pile.
  *
  * Constraints
  *
  * 1 <= n <= 100
  * 1 <= ar[i] <= 100 where 0 <= i < n
  *
  * Output Format
  *
  * Return the total number of matching pairs of socks that John can sell.
  *
  * From https://www.hackerrank.com/challenges/sock-merchant/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=warmup
  */
object SockMerchant {

  // Complete the sockMerchant function below.
  def sockMerchant(n: Int, ar: Array[Int]): Int = {
    ar.groupBy(i => i).
      map { case (i, vals) => (i, vals.size / 2) }.
      map { case (_, pairs) => pairs }.sum
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val n = stdin.readLine.trim.toInt

    val ar = stdin.readLine.split(" ").map(_.trim.toInt)
    val result = sockMerchant(n, ar)

    printWriter.println(result)

    printWriter.close()
  }
}
