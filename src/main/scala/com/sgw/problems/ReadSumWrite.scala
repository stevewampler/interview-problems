package com.sgw.problems

/**
 * App that reads from stdin, sums two integers, and writes the sum to stdout.
 * The first line of stdin contains the number of following lines.
 * E.g.:
 * 3
 * 1 5
 * 3 10
 * 999 -34343
 */
object ReadSumWrite extends App {
  val n = readInt()
  (1 to n).map(_ => readLine().split(" ").map(_.toInt).sum).foreach(println)
}
