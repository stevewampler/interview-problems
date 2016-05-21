package com.sgw.problems

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

import scala.io.Source

class DijkstraShortestReachSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "The DijkstraShortestReach" should "return the right results for a small graph" in {
    DijkstraShortestReach.solve("/problems/graph/dijkstra/test_case_1_input.txt") should be (List(
      List(24, 3, 15)
    ))
  }

  "The DijkstraShortestReach" should "return the right results for a medium size graph" in {
    DijkstraShortestReach.solve("/problems/graph/dijkstra/test_case_2_1_input.txt").map(_.mkString(" ")) should be (
      Source.fromInputStream(getClass.getResourceAsStream("/problems/graph/dijkstra/test_case_2_1_output.txt")).getLines().toList
    )
  }

  "The DijkstraShortestReach" should "return the right results for a large graph" in {
    DijkstraShortestReach.solve("/problems/graph/dijkstra/test_case_2_input.txt").map(_.mkString(" ")) should be (
      Source.fromInputStream(getClass.getResourceAsStream("/problems/graph/dijkstra/test_case_2_output.txt")).getLines().toList
    )
  }
}
