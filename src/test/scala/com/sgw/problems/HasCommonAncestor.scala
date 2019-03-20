package com.sgw.problems

// Suppose we have some input data describing a graph of relationships between parents and children over multiple generations. The data is formatted as a list of (parent, child) pairs, where each individual is assigned a unique integer identifier.
// For example, in this diagram, 3 is a child of 1 and 2, and 5 is a child of 4:
// 1   2   4
//  \ /   / \
//   3   5   8
//    \ / \   \
//     6   7   10
// Write a function that takes the graph, as well as two of the individuals in our dataset, as its inputs and returns true if and only if they share at least one ancestor.
// Sample input and output:
// hasCommonAncestor(parentChildPairs, 3, 8) => false
// hasCommonAncestor(parentChildPairs, 5, 8) => true
// hasCommonAncestor(parentChildPairs, 6, 8) => true
// hasCommonAncestor(parentChildPairs, 1, 3) => false
//
// What I learned:
// - coding under pressure with someone watching is hard
// - my mind went blank and just couldn't figure out how to build the ancestor set ... simple recursion did it (below)
// - coding in a browser window :(
// - gotta somehow practice more under pressure

import scala.collection._

object HasCommonAncestor extends App {
  // Scala
  val parentChildPairs = List((1, 3), (2, 3), (3, 6), (5, 6),
    (5, 7), (4, 5), (4, 8), (8, 10))

  def hasCommonAncestor(list: List[(Int, Int)], n1: Int, n2: Int): Boolean = {
    val child2ParentsMap = list.foldLeft(Map[Int, Set[Int]]()) { case (acc, (parent, child)) =>
      acc.updated(child, acc.getOrElse(child, Set()) + parent)
    }

    def getAncestors(n: Int): Set[Int] = {
      val parents = child2ParentsMap.getOrElse(n, Set[Int]())

      parents.foldLeft(parents) { case (acc, p) =>
        acc ++ getAncestors(p)
      }
    }

    val a1: Set[Int] = getAncestors(n1)
    val a2: Set[Int] = getAncestors(n2)
    val a3: Set[Int] = a1.intersect(a2)

    a3.nonEmpty
  }

  assert(!hasCommonAncestor(parentChildPairs, 3, 8))
  assert( hasCommonAncestor(parentChildPairs, 5, 8))
  assert( hasCommonAncestor(parentChildPairs, 6, 8))
  assert(!hasCommonAncestor(parentChildPairs, 1, 3))
  assert( hasCommonAncestor(parentChildPairs, 6, 6))
  assert(!hasCommonAncestor(parentChildPairs, 6, 20))
}
