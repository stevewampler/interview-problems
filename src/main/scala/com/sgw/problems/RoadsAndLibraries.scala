package com.sgw.problems

import java.io.{BufferedReader, InputStreamReader}

import scala.collection.mutable
import scala.tools.nsc.interpreter.InputStream

/**
  * See https://www.hackerrank.com/challenges/torque-and-development/problem?h_l=interview&isFullScreen=false&playlist_slugs%5B%5D%5B%5D%5B%5D%5B%5D%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D%5B%5D%5B%5D%5B%5D%5B%5D=graphs
  */
object RoadsAndLibraries {

  // Complete the roadsAndLibraries function below.
  def roadsAndLibraries(n: Int, c_lib: Long, c_road: Long, cities: Array[Array[Int]]): Long = {
    // if libraries cost less to build than roads ...
    if (c_lib <= c_road) {
      // build a library at each city
      n * c_lib
    } else { // build one library for each connected set of cities and connect them with roads
      // group cities based on how they're connected
      val cityPartitions: List[Set[Int]] = partitionCities2(
        n,
        cities.toList.map { case Array(city1, city2) =>
          (city1, city2)
        }
      )

//      println(cityPartitions)

      val numLibraries = cityPartitions.size // build one lib. per city partition

      val numRoads = cityPartitions.map { cityPartition =>
        // returns the number of roads to built between cities in the partition
        cityPartition.size - 1
      }.sum // add up all the roads

//      println(numLibraries)
//      println(numRoads)

      numLibraries * c_lib + numRoads * c_road
    }
  }

  private def toMap(n: Int, pairs: List[(Int, Int)]): Map[Int, Set[Int]] = {
    val map1: Map[Int, Set[Int]] = pairs.groupBy { pair =>
      pair._1
    }.map { case (i, pairs) =>
      i -> pairs.map { case (_, i2) =>
        i2
      }.toSet
    }

    val map2: Map[Int, Set[Int]] = pairs.groupBy { pair =>
      pair._2
    }.map { case (i, pairs) =>
      i -> pairs.map { case (i1, _) =>
        i1
      }.toSet
    }

    val map = (1 to n).map { i =>
      val set1: Set[Int] = map1.getOrElse(i, Set())
      val set2: Set[Int] = map2.getOrElse(i, Set())

      i -> (set1 ++ set2)
    }.toMap

//    println(s"map = $map")

    map
  }

//  // using a stack to keep track of the nodes we need to work on
//  private val stack = mutable.Stack[Int]()

//  // collects all of the nodes associated with the ith node
//  private def collectSet(i: Int, map: Map[Int, Set[Int]], visited: mutable.Set[Int]): mutable.Set[Int] = {
//    // this is the set of nodes we'll return
//    val set: mutable.Set[Int] = mutable.Set[Int]()
//
//    // push the root node onto the stack
//    stack.push(i)
//
//    // loop while we still have nodes to work on ...
//    while (stack.nonEmpty) {
//      // get the next node to work on
//      val j = stack.pop()
//
//      // if we haven't already visited the node ...
//      if (!visited.contains(j)) {
//        // add it to the set
//        set.add(j)
//
//        // push all of j's child nodes onto the stack
//        stack.pushAll(map.getOrElse(j, Set.empty[Int]))
//
//        // mark the jth node as visited
//        visited.add(j)
//      }
//    }
//
//    //    println(s"set = ${set.toSet}")
//
//    set
//  }

  // collects all of the nodes associated with the ith node
  private def collectSet(root: Int, map: Map[Int, Set[Int]], set: Set[Int]): Set[Int] = {
    map.getOrElse(root, Set()).foldLeft(set) { case (set, child) =>
      if (set.contains(child)) {
        set
      } else {
        collectSet(child, map, set + child)
      }
    }
  }

  private def collectSets(n: Int, map: Map[Int, Set[Int]]): List[Set[Int]] = {
    (1 to n).foldLeft(List[Set[Int]](), Set[Int]()) { case ((listOfSets, visited), i) =>
      if (visited.contains(i)) {
        (listOfSets, visited)
      } else {
        val set = collectSet(i, map, Set[Int](i))
        (set :: listOfSets, visited ++ set)
      }
    }._1
  }

  private def partitionCities2(n: Int, pairs: List[(Int, Int)]): List[Set[Int]] = collectSets(n, toMap(n, pairs))



  def runFromInputStream(inputStream: InputStream): Seq[Long] = {

    val reader = new BufferedReader(new InputStreamReader(inputStream))

    val q = reader.readLine.trim.toInt

    (1 to q).map { _ =>
      val nmC_libC_road = reader.readLine.split(" ")

      val n = nmC_libC_road(0).trim.toInt

      val m = nmC_libC_road(1).trim.toInt

      val c_lib = nmC_libC_road(2).trim.toLong

      val c_road = nmC_libC_road(3).trim.toLong

      val cities = Array.ofDim[Int](m, 2)

      for (i <- 0 until m) {
        cities(i) = reader.readLine.split(" ").map(_.trim.toInt)
      }

      roadsAndLibraries(n, c_lib, c_road, cities)
    }
  }
}

