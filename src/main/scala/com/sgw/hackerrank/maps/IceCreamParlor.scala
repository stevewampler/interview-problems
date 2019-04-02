package com.sgw.hackerrank.maps

object IceCreamParlor {

  def whatFlavors(cost: Array[Int], money: Int): (Int, Int) = {

    var i = 0
    var j = 1
    var done = false

    while (i < cost.length - 1 && !done) {
      j = i + 1

      while (j < cost.length && (cost(i) + cost(j)) != money) {
        j = j + 1
      }

      done = j < cost.length

      if (!done) {
        i = i + 1
      }
    }

    assert(done)

    (i + 1, j + 1)
  }

  def main(args: Array[String]) {
    val result1 = whatFlavors(Array(1, 4, 5, 3, 2), 4)
    println(result1)
    assert(result1 == (1, 4))
  }
}

