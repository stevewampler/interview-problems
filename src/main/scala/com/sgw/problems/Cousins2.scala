package com.sgw.problems

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
  type Node = Cousins.Node

  def relationship(node1: Node, node2: Node): Option[(Int, Int)] = relationship(Some(node1), Some(node2))

  private def relationship(maybeNode1: Option[Node], maybeNode2: Option[Node]): Option[(Int, Int)] = hopsToLCA(maybeNode1, maybeNode2).map {
    case (h1, h2) => (h1.min(h2) - 1, (h1-h2).abs)
  }

  private def nodeToList(maybeNode: Option[Node], list: List[Node] = Nil): List[Node] =
    maybeNode.map(node => nodeToList(node.maybeParent, node :: list)).getOrElse(list)

  private def hopsToLCA(maybeNode1: Option[Node], maybeNode2: Option[Node]): Option[(Int, Int)] = {
    val pathToNode1 = nodeToList(maybeNode1)
    val pathToNode2 = nodeToList(maybeNode2)

    // if the nodes are related, then they'll have the same root node
    if (pathToNode1.headOption != pathToNode2.headOption) {
      return None
    }

    // calculate the list of nodes that are the same between node1 and node2
    val sameList = pathToNode1.zip(pathToNode2).takeWhile {
      case (node1, node2) => node1 == node2
    }

    Some(pathToNode1.length - sameList.length, pathToNode2.length - sameList.length)
  }
}
