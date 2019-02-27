package com.sgw.problems

import java.io.{BufferedReader, InputStreamReader}

import scala.tools.nsc.interpreter.InputStream

/**
  * You're given n cities and m possible connections/roads that can be built between those cities.
  * You're also given the cost of building a library in a city (c_lib) and the cost of building
  * a road between two cities (c_road). The goal is to minimize the cost of building libraries and roads
  * such that every city either contains a library or is connected to another city that contains a library.
  *
  * Strategy:
  *
  * If it costs less to build a library than it does to build a road, then just build libraries in all of the cities.
  * If it costs more to build a library than to build a road, then, for each connected set of cities,
  * build a single library and then connect all of the cities in the set together to give each city access to the
  * library. It doesn't matter which city contains the library b/c the cost of building roads between cities is
  * a constant.
  *
  * From HackerRank: https://www.hackerrank.com/challenges/torque-and-development/problem?h_l=interview&isFullScreen=false&playlist_slugs%5B%5D%5B%5D%5B%5D%5B%5D%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D%5B%5D%5B%5D%5B%5D%5B%5D=graphs
  */
object RoadsAndLibraries {

  def roadsAndLibraries(n: Int, c_lib: Long, c_road: Long, cities: Array[Array[Int]]): Long = {
    // if libraries cost less to build than roads ...
    if (c_lib <= c_road) {
      // build a library at each city
      n * c_lib
    } else { // build one library for each connected set of cities and connect them with roads
      // group cities into a list of sets based on how they're connected
      val nodeSets: List[Set[Int]] = collectNodeSets(
        n,
        toAdjacentNodesMap(
          cities.toList.map { case Array(city1, city2) =>
            (city1, city2)
          }
        )
      )

      val numLibraries = nodeSets.size // build one lib. per connected set of cities

      val numRoads = nodeSets.map { nodeSet =>
        // return the number of roads to built between the connected cities
        nodeSet.size - 1
      }.sum // add up all the roads

      numLibraries * c_lib + numRoads * c_road
    }
  }

  // creates a node-to-adjacent-nodes map from the list of edges
  private def toAdjacentNodesMap(edges: List[(Int, Int)]): Map[Int, Set[Int]] =
    edges.foldLeft(Map[Int, Set[Int]]()) { case (map, (i1, i2)) =>
      map.updated(i1, map.getOrElse(i1, Set()) + i2).updated(i2, map.getOrElse(i2, Set()) + i1)
    }

  // recursively collects all of the nodes connected to the specified node into a set of connected nodes
  private def collectNodeSet(node: Int, adjacentNodesMap: Map[Int, Set[Int]], set: Set[Int]): Set[Int] =
    adjacentNodesMap.getOrElse(node, Set()).foldLeft(set) { case (set, child) =>
      if (set.contains(child)) {
        set
      } else {
        collectNodeSet(child, adjacentNodesMap, set + child)
      }
    }

  // finds all of the connected sets of nodes
  private def collectNodeSets(n: Int, map: Map[Int, Set[Int]]): List[Set[Int]] =
    (1 to n).foldLeft(List[Set[Int]](), Set[Int]()) { case ((listOfSets, visited), i) =>
      if (visited.contains(i)) {
        (listOfSets, visited)
      } else {
        val set = collectNodeSet(i, map, Set[Int](i))
        (set :: listOfSets, visited ++ set)
      }
    }._1



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

