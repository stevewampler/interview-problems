package com.sgw.problems

import com.sgw.problems.Cousins.Node

/**
 * Write a function that, given two tree nodes, will return the node's relationship (if any) in terms of "degree" and how far removed
 * they are from each other.
 *
 * n1 = node 1
 * n2 = node 2
 * lca = lowest-common ancestor of n1 and n2
 * h1 = the number of "hops" you have to take up the tree to get to n1's and n2's lowest-common ancestor
 * degree = min(h1, h2) -1 where h1 is the number of hops from n1 to the lca, and h2 is the number of hops from n2 to the lca
 * removed = | h1 - h2 |
 *
 * For example:
 *
 * Given the following two trees rooted at notes A and H:
 *
 *     A        H
 *   B   C    I   J
 *   D   E
 *   F   G
 *      X Y
 *
 * The lowest-common ancestor of F and E is A.
 * The lowest-common ancestor of F and I is undefined.
 *
 * B and C are siblings of A. They are both one hop from A, so:
 *   degree = min(1, 1) - 1 = 0 => or zeroth degree cousins
 *   removed = | 1 - 1 | = 0
 *
 * F is the great grandchild of A (h1 = 3) and E is the grandchild of A (h2 = 2), so:
 *   degree = min(3, 2) - 1 = 1 => 1st cousins
 *   removed = | 3 - 2 | = 1 => once removed
 *
 * F is the great grandchild of A (h1 = 3) and Y is the great-great grandchild of A (h2 = 4), so:
 *   degree = min(3, 4) - 1 = 2 => 2nd cousins
 *   removed = | 3 - 4 | = 1 => once removed
 *
 * D is the grandchild of A (h1 = 2) and Y is the great-great grandchild of A (h2 = 4), so:
 *   degree = min(2, 4) - 1 = 1 => 1st cousins
 *   removed = | 2 - 4 | = 2 => twice removed
 *
 */
object Cousins2 {
  object Node {
    def apply(name: String, parent: Node): Node = Node(name, Option(parent))
  }

  case class Node(name: String, maybeParent: Option[Node] = None) {
    def toList(list: List[Node] = List()): List[Node] =
      maybeParent.map { parent =>
        parent.toList(this :: list)
      }.getOrElse(this :: list)
  }

  def relationship(node1: Node, node2: Node): Option[(Int, Int)] = hopsToLCA(node1, node2).map {
    case (h1, h2) => (h1.min(h2) - 1, (h1-h2).abs)
  }

  private def hopsToLCA(node1: Node, node2: Node): Option[(Int, Int)] = {
    val list1 = node1.toList()
    val list2 = node2.toList()

    val commonLength = list1.zip(list2).takeWhile { case (n1, n2) =>
      n1 == n2
    }.size

    if (commonLength > 0) {
      Some(list1.size - commonLength, list2.size - commonLength)
    } else {
      None
    }
  }
}
