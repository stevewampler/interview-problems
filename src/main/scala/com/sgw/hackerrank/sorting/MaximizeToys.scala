package com.sgw.hackerrank.sorting

/**
  * From https://www.hackerrank.com/challenges/mark-and-toys/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=sorting&h_r=next-challenge&h_v=zen
  *
  * Given an array of toy prices and a max amount to spend, figure out the maximum number of toys you can buy.
  *
  * Strategy: Sort the prices and then consume them until you've spent all that you can.
  */
object MaximizeToys {

  def maximumToys(prices: Array[Int], k: Int): Int = {

    val n = prices.length
    val sortedPrices = prices.toList.sorted

    var total = 0

    val toys = sortedPrices.takeWhile { price =>
      total = total + price
      total <= k
    }

    toys.size
  }

  def main(args: Array[String]) {
    assert(maximumToys(Array(1, 12, 5, 111, 200, 1000, 10), 50) == 4)
  }
}