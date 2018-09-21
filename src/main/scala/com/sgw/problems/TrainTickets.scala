package com.sgw.problems

/**
  * There are N stations in a route, starting from 0 to N-1. A train moves from first station (0) to
  * last station (N-1) in only forward direction.
  * The cost of ticket between any two stations is given.
  * Find the minimum cost of travel from station 0 to station N-1.
  *
  * Meenakshi. Dynamic Programming for Coding Interviews:
  * A Bottom-Up approach to problem solving (p. 36). Notion Press. Kindle Edition.
  */
object TrainTickets extends App {

  // the cost to travel from the ith station (row) to the jth station (column)
  val tickets: Array[Array[Int]] = Array(
    Array( 0, 10, 75, 95, 100), // the cost to travel from station 0 to stations 1, 2, 3, and 4
    Array(-1,  0, 15, 55,  50), // the cost to travel from station 1 to stations 2, 3, and 4
    Array(-1, -1,  0, 50,  30), // the cost to travel from station 2 to stations 3, and 4
    Array(-1, -1, -1,  0,  10), // the cost to travel from station 3 to station 4
    Array(-1, -1, -1, -1,   0)
  )

  // the number of stations
  val n = tickets.length

  /**
    * Calculates the minimum cost to go from fromStation to toStation.
    *
    * Strategy: divide and conquer
    *
    * To calc. the min cost to go from station i to j, this method calculate the costs to go from station
    * i to k and station k to j for k between i+1 (inclusive) to j (exclusive) and then folds over the costs
    * to calculate the min cost, starting with the cost to go directly from i to j.
    *
    * This solution is a bit brute force in it calculates sub-solutions multiple times.
    *
    * @param fromStation the index of the station we're traveling from
    * @param toStation the index of the station we're traveling to
    *
    * @return the minimum cost to travel from the fromStation to the toStation
    */
  def minCost1(fromStation: Int, toStation: Int): Int = {
    println(s"$fromStation -> $toStation")
//    ((fromStation + 1) until toStation).foldLeft(tickets(fromStation)(toStation)) { case (acc, mid) =>
//      acc.min(minCost1(fromStation, mid) + minCost1(mid, toStation))
//    }

    ((fromStation + 1) until toStation).map { midStation =>
      minCost1(fromStation, midStation) + minCost1(midStation, toStation)
    }.foldLeft(tickets(fromStation)(toStation)) { case (acc, cost) =>
      acc.min(cost)
    }
  }

  // memoized version

  val mins: Array[Array[Int]] = Array.ofDim(n, n)

  (0 until n).foreach { i =>
    (0 until n).foreach { j =>
      mins(i)(j) = Int.MaxValue
    }
  }

  def minCost2(fromStation: Int, toStation: Int, min: Int = Int.MaxValue): Int = {

    if (mins(fromStation)(toStation) != Int.MaxValue) {
      mins(fromStation)(toStation)
    } else {
      println(s"$fromStation -> $toStation")
      val newMin = ((fromStation + 1) until toStation).map { midStation =>
        minCost2(fromStation, midStation) + minCost2(midStation, toStation)
      }.foldLeft(tickets(fromStation)(toStation)) { case (acc, cost) =>
        acc.min(cost)
      }

      mins(fromStation)(toStation) = newMin

      newMin
    }
  }

  println(minCost1(0, n-1))
  println(minCost2(0, n-1))
}
