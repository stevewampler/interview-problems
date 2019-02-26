package com.sgw.problems

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

/**
  * TODO: Not finished yet.
  *
From: https://www.hackerrank.com/challenges/dijkstrashortreach

Given a graph consisting of N nodes (labelled 1 to N) where a specific node S represents the starting position S and
an edge between two nodes is of a given length, which may or may not be equal to other lengths in the graph.

Calculate the shortest distance from the start position (Node S) to all of the other nodes in the graph.

Note 1: If a node is unreachable , the distance is assumed as −1.

Input Format

The first line contains T, denoting the number of test cases.
First line of each test case has two integers N, denoting the
number of nodes in the graph and M, denoting the number of edges in the graph.

The next M lines each consist of three space-separated integers x y r, where x and y denote the two nodes between
which the undirected edge exists, r denotes the length of edge between these corresponding nodes.

The last line has an integer S, denoting the starting position.

Constraints
1≤T≤10
2≤N≤3000
1≤M≤N×(N−1))/2
1≤x,y,S≤N
1≤r≤350

If there are edges between the same pair of nodes with different weights, they are to be considered as is, like multiple edges.

Output Format

For each of the T test cases, print a single line consisting N−1 space separated integers denoting the shortest distance
of N−1 nodes from starting position S.

For unreachable nodes, print −1.

Sample Input

1
4 4
1 2 24
1 4 20
3 1 3
4 3 12
1

Sample Output

24 3 15

Explanation

The graph given in the test case is shown as :

S -- 24 -- B
S -- 20 -- D
S --  3 -- C -- 12 -- D

The straight line is a weighted edge, denoting length of edge between the corresponding nodes.
The nodes S,B,C and D denote the obvious node 1,2,3 and 4 in the test case.
The shortest paths followed for the three nodes B,C and D are as follows :

S->B - Shortest Path Value : 24

S->C - Shortest Path Value : 3

S->C->D - Shortest Path Value : 15
  */
object DijkstraShortestReach {

  def main(args: Array[String]) {
    val numProb = readLine().trim.toInt
    // println(s"numProbl $numProb")
    val results = (0 until numProb).toIterator.map(_ => {
      // println("reading n & m")
      readLine().trim
    }).map(_.split(" ").toList).map(arr => (arr(0).toInt, arr(1).toInt)).map {
      case (n, m) => {
        val nodes = (1 to n).toList
        // println("reading edges")
        val edges = (1 to m).toIterator.map(_ => readLine().trim).map(_.split(" ").toList).
          map(arr => (arr(0).toInt, arr(1).toInt, arr(2).toInt)).toList
        // println(s"edges = $edges")
        // println("reading s")
        val s = readLine().trim.toInt
        // println(s"s=$s")

        solve(nodes, edges, s)
      }
    }.toList

    println(results.mkString("\n"))
  }

  def solve(resourceURL: String): List[List[Int]] =
    solve(Source.fromInputStream(getClass.getResourceAsStream(resourceURL)).getLines().toList)

  def solve(lines: List[String]): List[List[Int]] = {
    val iter = lines.toIterator

    val numProb = iter.next().trim.toInt

    (0 until numProb).toIterator.map(_ => iter.next().trim).map(_.split(" ").toList).map(arr => (arr(0).toInt, arr(1).toInt)).map {
      case (n, m) => {
        val nodes = (1 to n).toList
        // println("reading edges")
        val edges = (1 to m).toIterator.map(_ => iter.next().trim).map(_.split(" ").toList).map(arr => (arr(0).toInt, arr(1).toInt, arr(2).toInt)).toList
        // println(s"edges = $edges")
        // println("reading s")
        val s = iter.next().trim.toInt
        // println(s"s=$s")

        solve(nodes, edges, s)
      }
    }.toList
  }

  // O(n + m + n^2) => O(n^2)
  def solve(ns: List[Int], es: List[(Int, Int, Int)], s: Int): List[Int] = {
    // O(n^2) for fully connected graphs, O(1) for a fully disconnected graph
    @tailrec
    def go(acc: Map[Node, Int], queue: Queue[(Node, Int)]): Map[Node, Int] = {
      if (queue.isEmpty) return acc

      val ((node, dist), queue2) = queue.dequeue

      val (newQueue, newAcc) = node.edges.
        map(edge => (edge.otherNode, edge.length)).
        filter {
          case (otherNode, length) => dist + length < acc.getOrElse(otherNode, Int.MaxValue)
        }.
        foldLeft((queue2, acc)) {
          case ((queue3, acc3), (otherNode, length)) => {
            (
              queue3.enqueue((otherNode, dist + length)),
              acc3.updated(otherNode, dist + length)
            )
          }
        }

      go(newAcc, newQueue)
    }

    // create a collection of nodes and edges
    val nodes = createGraph(ns, es) // O(n + m)

    // get the starting node
    val startNode = nodes(s - 1) // O(1)

    // prime the result and the queue with the starting node
    val acc = go(Map[Node, Int](startNode -> 0), Queue[(Node, Int)](startNode -> 0)) // O(m)

    // generate the results filtering out the start node
    nodes.map(node => acc.getOrElse(node, -1)).filter(_ != 0) // O(n)
  }

  class Node(val i: Int, var edges: List[Edge] = List()) {
    def add(edge: Edge): Unit = edges = edge :: edges

    override def toString: String = s"Node: $i  ${edges.map(edge => edge.otherNode.i + ":" + edge.length).mkString(",")}"
  }

  case class Edge(otherNode: Node, length: Int)

  def createGraph(ns: List[Int], es: List[(Int, Int, Int)]): List[Node] = {
    val nodes = ns.map(i => new Node(i))

    es.foreach {
      case (nodeIndex1, nodeIndex2, edgeLength) => {
        val n1 = nodes(nodeIndex1 - 1)
        val n2 = nodes(nodeIndex2 - 1)
        n1.add(Edge(n2, edgeLength))
        n2.add(Edge(n1, edgeLength))
      }
    }

    nodes
  }
}
