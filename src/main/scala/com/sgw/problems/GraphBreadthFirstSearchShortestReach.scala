package com.sgw.problems

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

/**
  * Given an undirected graph consisting of NN nodes (labelled 1 to N) where a specific given node SS represents the start position and an edge between any two nodes is of length 66 units in the graph.
 **
  * It is required to calculate the shortest distance from start position (Node S) to all of the other nodes in the graph.
 **
  * Note 1: If a node is unreachable , the distance is assumed as −1−1.
*Note 2: The length of each edge in the graph is 66 units.
 **
  * Input Format
 **
  * The first line contains TT, denoting the number of test cases.
*First line of each test case has two integers NN, denoting the number of nodes in the graph and MM, denoting the number of edges in the graph.
*The next MM lines each consist of two space separated integers x yx y, where xx and yy denote the two nodes between which the edge exists.
*The last line of a testcase has an integer SS, denoting the starting position.
 **
  * Constraints
*1≤T≤101≤T≤10
*2≤N≤10002≤N≤1000
*1≤M≤N×(N−1)21≤M≤N×(N−1)2
*1≤x,y,S≤N1≤x,y,S≤N
 **
  * Output Format
 **
  * For each of TT test cases, print a single line consisting of N−1N−1 space-separated integers, denoting the shortest distances of the N-1 nodes from starting position SS. This will be done for all nodes same as in the order of input 1 to N.
 **
  * For unreachable nodes, print −1−1.
 **
  * Sample Input
 **
  * 2
*4 2
*1 2
*1 3
*1
*3 1
*2 3
*2
*Sample Output
 **
  * 6 6 -1
*-1 6
*Explanation
 **
  * For test cases 1:
 **
  * The graph given in the test case is shown as :
 **
  * S <-> B
  *<-> C
*D
 **
  * S denotes the node 1 in the test case and B,C and D denote 2,3 and 4. Since S is the starting node and the shortest distances from it are (1 edge, 1 edge, Infinity) to the nodes B,C and D (2,3 and 4) respectively.
 **
  * Node D is unreachable, hence -1 is printed (not Infinity).
 **
  * For test cases 2: There are only one edge (2, 3) in a graph with 3 nodes, so node 1 is unreachable from node 2, and node 3 has one edge from node 2, each edge has the length of 6 units. So we output -1 6.
 *
 * Another Test Case
  * Input:
*1
*7 6
*1 2
*1 3
*2 3
*2 5
*2 4
*4 5
*1
 **
  * Output:
 **
  * 6 6 12 12 -1 -1
  */
object GraphBreadthFirstSearchShortestReach {

//  def main(args: Array[String]) {
//    val numProb = readLine().trim.toInt
//    // println(s"numProbl $numProb")
//    val results = (0 until numProb).toIterator.map(_ => {
//      // println("reading n & m")
//      readLine().trim
//    }).map(_.split(" ").toList).map(arr => (arr(0).toInt, arr(1).toInt)).map {
//      case (n, m) => {
//        val nodes = (1 to n).toList
//        // println("reading edges")
//        val edges = (1 to m).toIterator.map(_ => readLine().trim).map(_.split(" ").toList).map(arr => (arr(0).toInt, arr(1).toInt)).toList
//        // println(s"edges = $edges")
//        // println("reading s")
//        val s = readLine().trim.toInt
//        // println(s"s=$s")
//
//        solve(nodes, edges, s)
//      }
//    }.toList
//
//    println(results.mkString("\n"))
//  }

  def solve(resourceURL: String): List[List[Int]] =
    solve(Source.fromInputStream(getClass.getResourceAsStream(resourceURL)).getLines().toList)

  def solve(lines: List[String]): List[List[Int]] = {
    val iter = lines.toIterator

    val numProb = iter.next().trim.toInt

    (0 until numProb).toIterator.map(_ => iter.next().trim).map(_.split(" ").toList).map(arr => (arr(0).toInt, arr(1).toInt)).map {
      case (n, m) => {
        val nodes = (1 to n).toList
        // println("reading edges")
        val edges = (1 to m).toIterator.map(_ => iter.next().trim).map(_.split(" ").toList).map(arr => (arr(0).toInt, arr(1).toInt)).toList
        // println(s"edges = $edges")
        // println("reading s")
        val s = iter.next().trim.toInt
        // println(s"s=$s")

        solve(nodes, edges, s)
      }
    }.toList
  }

  def solve(ns: List[Int], es: List[(Int, Int)], s: Int): List[Int] = {
    @tailrec
    def go(acc: Map[Node, Int], queue: Queue[(Node, Int)], enqueued: Set[Node]): Map[Node, Int] = {
      if (queue.isEmpty) return acc

      val ((node, dist), queue2) = queue.dequeue

      val (newQueue, newEnqueued) = node.ns.filter(node => !enqueued.contains(node)).foldLeft((queue2, enqueued)) {
        case ((queue, enqueued), node) => {
          (queue.enqueue((node, dist + 6)), enqueued + node)
        }
      }

      val newAcc = acc.updated(node, dist)

      go(newAcc, newQueue, newEnqueued)
    }

    val nodes = createNodes(ns, es)

    val startNode = nodes(s - 1)

    val acc = go(Map[Node, Int](startNode -> 0), Queue[(Node, Int)]((startNode, 0)), Set(startNode))

    nodes.map(node => acc.getOrElse(node, -1)).filter(_ != 0)
  }

  class Node(val i: Int, var ns: Set[Node] = Set()) {
    def add(node: Node): Unit = if (node != this) { ns = ns + node }

    override def toString: String = s"Node: $i  ${ns.map(n => n.i).mkString(",")}"
  }

  def createNodes(ns: List[Int], es: List[(Int, Int)]): List[Node] = {
    val nodes = ns.map(i => new Node(i))

    es.foreach {
      case (n1, n2) => {
        nodes(n1 - 1).add(nodes(n2 - 1))
        nodes(n2 - 1).add(nodes(n1 - 1))
      }
    }

    nodes
  }
}
