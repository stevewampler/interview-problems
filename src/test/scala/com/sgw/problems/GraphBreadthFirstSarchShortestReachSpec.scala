package com.sgw.problems

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class GraphBreadthFirstSearchShortestReachSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "The GraphBreadthFirstSearchShortestReach" should "return the right results for a small graph" in {
    GraphBreadthFirstSearchShortestReach.solve("/small_graph.txt") should be (List(
      List(6, 6, 12, 12, -1, -1)
    ))
  }

  "The GraphBreadthFirstSearchShortestReach" should "return the right results for a large graph" in {
    GraphBreadthFirstSearchShortestReach.solve("/large_graph.txt").map(_.mkString(" ")) should be (List(
      "6 6 6 6 12 6 12 6 12 12 6 6 6 6 6 12 12 6 6 6 6 12 6 12 6 12 6 12 12 12 12 6 12 12 6 12 12 6 12 6 12 6 12 12 6 6 12 6 6 6 6 12 12 12 12 6 6 6 12 6 6 12 12 12 12 12 12 6 6"
    ))
  }
}