package com.sgw.hackerrank.greedyalgos

import java.io.{BufferedReader, InputStream, InputStreamReader}

/**
  * From https://www.hackerrank.com/challenges/luck-balance/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=greedy-algorithms&h_r=next-challenge&h_v=zen
  */
object LuckBalance {

  def luckBalance(k: Int, contests: Array[Array[Int]]): Int = {
    val (important, unimportant) = contests.partition {
      case Array(_, importance) => importance == 1
    }

    val importantSorted = important.sortBy { case Array(luck, _) =>
      -luck
    }

    val importantLost = importantSorted.take(k)

    val importantWon = importantSorted.drop(k)

    unimportant.map { case Array(luck, _) => luck }.sum +
      importantLost.map { case Array(luck, _) => luck }.sum -
      importantWon.map { case Array(luck, _) => luck }.sum
  }

  def run(inputStream: InputStream): Int = {
    val reader = new BufferedReader(new InputStreamReader(inputStream))

    val nk = reader.readLine.split(" ")

    val n = nk(0).trim.toInt

    val k = nk(1).trim.toInt

    val contests = Array.ofDim[Int](n, 2)

    for (i <- 0 until n) {
      contests(i) = reader.readLine.split(" ").map(_.trim.toInt)
    }

    luckBalance(k, contests)
  }
}

