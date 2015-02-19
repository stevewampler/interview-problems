package com.sgw.problems

/**
 * author: steve
 */
object TravelingChef {
  case class Edge(id: Int, v:Node, s: Int, e: Int)
  case class Node(id: Int, var edges: List[Edge])

  /**
   * A chef has to get from bus stop 1 to n within a specified time period t. He must arrive at a bus stop before
   * the bus leaves (not when it leaves). I.e. he has to wait at least 1 minute. We want to minimize the
   * maximum time he waits at any one bus stop.
   *
   * @param n number of bus stations
   * @param t how long (time) the chef has to get from station 1 to n
   * @param m the number of buses
   * @param schedule the bus schedule as a tuple (starting-bus-stop, ending-bus-stop, start-time, end-time)
   *
   * @return the maximum period of time the chef must wait for a bus at any one stop
   */
  def optimize(n: Int, t: Int, m: Int, schedule: Seq[(Int, Int, Int, Int)]): Int = {
    val nodes = (1 to n).map(id => Node(id, Nil))

    println("Nodes: " + nodes)

    val edges = schedule.zipWithIndex.map {
      case ((u, v, s, e), id) => {
        val un = nodes(u-1)
        val vn = nodes(v-1)

        val edge = Edge(id + 1, nodes(v-1), s, e)

        un.edges = edge :: un.edges

        edge
      }
    }

    println("Edges: " + edges)
    println("Nodes: " + nodes)

    def findPaths(node: Node, currentTime: Int = 0): List[List[Edge]] = {
      node.edges
        .filter(edge => edge.s > currentTime)
        .flatMap(edge => findPaths(edge.v, edge.e) match {
          case list: List[List[Edge]] if list.isEmpty => List(List(edge))
          case list: List[List[Edge]] => list.map(path => edge :: path)
        })
    }

    val paths = findPaths(nodes(0))

    println("Paths:")
    paths.foreach(path => println(path.map(edge => edge.id).mkString(",")))

    def maxWaitTime(path: List[Edge]): (Int, Int) =
      path.foldLeft((0, Integer.MIN_VALUE)) {
        case ((currentTime, maxWaitTime), edge) => (edge.e, maxWaitTime.max(edge.s - currentTime))
      }

    val totalAndMaxWaitTimes = paths.map(path => maxWaitTime(path))

    println("Total and Max Wait Times:")
    totalAndMaxWaitTimes.foreach(println)

    val filteredTotalAndMaxWaitTimes = totalAndMaxWaitTimes.filter {
      case (totalTime, _) => totalTime < t
    }

    val maxWaitTimes = filteredTotalAndMaxWaitTimes.map {
      case (_, maxWaitTime) => maxWaitTime
    }

    if (maxWaitTimes.isEmpty) -1 else maxWaitTimes.min
  }

  def main (args: Array[String]) {
    val minTime = optimize(5, 10, 5, List(
      (1, 2, 1, 2),
      (1, 5, 3, 4),
      (2, 4, 4, 5),
      (2, 5, 5, 6),
      (4, 5, 6, 7)
    ))

    println("Maximum time the chef must wait at any one stop: " + minTime)
  }
}
