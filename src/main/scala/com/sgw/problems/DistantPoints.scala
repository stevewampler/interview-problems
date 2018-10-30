package com.sgw.problems

import scala.util.Random
import scala.math._

case class Point(x: Double = Random.nextDouble(), y: Double = Random.nextDouble()) {
  def distance(p2: Point): Double = Math.sqrt(pow(x - p2.x, 2) + pow(y - p2.y, 2))
}

/**
  * Assume you have N number of random points on a 2D grid. Eliminate any two points that are
  * within D distance from each other.
  */
object DistantPoints {
  // O(n^2) but stops checking other points once a point has been eliminated
  def findValidPoints1(
    d: Double,
    points: List[Point]
  ): (List[Point], Int) = {
    var ops = 0

    (points.filter { p1 =>
      !points.exists {  p2 =>
        ops = ops + 1
        p2 != p1 && p1.distance(p2) <= d
      }
    }, ops)
  }

  // O(n^2) but avoids checking points that have already been eliminated
  def findValidPoints2(
    d: Double,
    points: Set[Point]
  ): Set[Point] = {
    val rejected = points.foldLeft(Set[Point]()) { (rejected, p1) =>
      if (rejected.contains(p1)) {
        rejected
      } else {
        val rejected2 = points.filter { p2 =>
          p1 != p2 && p1.distance(p2) <= d
        }

        if (rejected2.nonEmpty) {
          rejected ++ (rejected2 + p1)
        } else {
          rejected
        }
      }
    }

    points.diff(rejected)
  }

  // O(n * log(n))
  def findValidPoints3(
    d: Double,
    points: List[Point]
  ): (Set[Point], Int) = {
    var ops = 0

    // O(m * n)
    def findInvalidPoints(
      d: Double,
      check: List[Point],
      against: List[Point]
    ): List[Point] = check.filter { p1 =>
      against.exists { p2 =>
        ops = ops + 1
        p2 != p1 && p1.distance(p2) <= d
      }
    }

    // O(n * log(n))
    val sortedX = points.sortBy(_.x)

    // O(n), results in m points where m <= n
    val filteredX = sortedX.zip(sortedX.tail).map { case (p1, p2) =>
      (p1, p2) -> (p2.x - p1.x)
    }.filter { case (_, xDist) =>
      xDist <= d
    }.flatMap { case ((p1, p2), _) =>
      List(p1, p2)
    }.distinct

    println(s"filteredX.size = ${filteredX.size}")

    // O(m * log(m))
    val sortedY = filteredX.sortBy(_.y)

    // O(m), results in l points where l <= m
    val filteredY = sortedY.zip(sortedY.tail).map { case (p1, p2) =>
      (p1, p2) -> (p2.y - p1.y)
    }.filter { case ((p1, p2), yDist) =>
      yDist <= d
    }.flatMap { case ((p1, p2), _) =>
      List(p1, p2)
    }.distinct

    println(s"filteredY.size= ${filteredY.size}")

    // O(l * n)
    val invalidPoints = findInvalidPoints(d, filteredY, points).toSet

    println(s"invalidPoints.size = ${invalidPoints.size}")

    // O(l)
    (points.toSet.diff(invalidPoints), ops)
  }

  private def distances(points: List[Point]): List[Double] = points.flatMap { p1 =>
    points.filter(p2 => p2 != p1).map(p2 => p1.distance(p2))
  }

  def findValidPoints4(
    d: Double,
    points: List[Point]
  ): (Set[Point], Int) = {
    var ops: Int = 0

    val rejected = points.foldLeft(Set[Point]()) { case (rejected, p1) =>
      // if p1 has already been rejected, then no need to test it ...
      if (rejected.contains(p1)) {
        rejected
      } else {
        points.find { p2 =>
          ops = ops + 1
          p2 != p1 && p2.distance(p1) <= d
        }.map { p2 =>
          rejected + p1 + p2
        }.getOrElse {
          rejected
        }
      }
    }

    (points.toSet.diff(rejected), ops)
  }

  def main(args: Array[String]): Unit = {
    val n = 100
    val d = 0.1
    val points = (0 until n).map(_ => Point()).toList

    val (validPoints1, ops1) = findValidPoints1(d, points)
    println(s"ops1 = $ops1")
    println(s"validPoints1.size = ${validPoints1.size}")
    validPoints1.foreach(println)
    val distances1 = distances(validPoints1)
    println(s"distances1.size = ${distances1.size}")
    val filteredDistances1 = distances1.filter(_ <= d)
    println(s"filteredDistances1.size = ${filteredDistances1.size}")
    filteredDistances1.foreach(println)

    println()

//    val validPoints2 = findValidPoints2(d, points.toSet)
//    println(s"validPoints2.size = ${validPoints2.size}")
//    validPoints2.foreach(println)
//    val distances2 = distances(validPoints2.toList)
//    println(s"distances2.size = ${distances2.size}")
//    val filteredDistances2 = distances2.filter(_ <= d)
//    println(s"filteredDistances2.size = ${filteredDistances2.size}")
//    filteredDistances2.foreach(println)
//
//    println()
//
//    val validPoints3 = findValidPoints3(d, points)
//    println(s"validPoints3.size = ${validPoints3.size}")
//    validPoints3.foreach(println)
//    val distances3 = distances(validPoints3.toList)
//    println(s"distances3.size = ${distances3.size}")
//    val filteredDistances3 = distances3.filter(_ <= d)
//    println(s"filteredDistances3.size = ${filteredDistances3.size}")
//    filteredDistances3.foreach(println)
//
//    println()

    val (validPoints4, ops4) = findValidPoints4(d, points)
    println(s"ops4 = $ops4")
    println(s"validPoints4.size = ${validPoints4.size}")
    validPoints4.foreach(println)
    val distances4 = distances(validPoints4.toList)
    println(s"distances4.size = ${distances4.size}")
    val filteredDistances4 = distances4.filter(_ <= d)
    println(s"filteredDistances4.size = ${filteredDistances4.size}")
    filteredDistances4.foreach(println)

    assert(validPoints1.toSet == validPoints4)
  }
}
