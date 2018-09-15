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
object Cousins {
  object Node {
    def apply(name: String, parent: Node): Node = Node(name, Option(parent))
  }

  case class Node(name: String, maybeParent: Option[Node] = None)

  def relationship(n1: Node, n2: Node): Option[(Int, Int)] = hopsToLCA(n1, n2).map {
    case (h1, h2) => (h1.min(h2) - 1, (h1-h2).abs)
  }

  private def nodeToList(maybeNode: Option[Node]): List[Node] =
    maybeNode.map(n => n :: nodeToList(n.maybeParent)).getOrElse(Nil)

  private def nodeToHopMap(n: Node): Map[Node, Int] =
    nodeToList(Some(n)).zipWithIndex.toMap

  private def hopsToLCA(n2Opt: Option[Node], hop2: Int, hopMap: Map[Node, Int]): Option[(Int, Int)] =
    n2Opt.flatMap(n2 => hopMap.get(n2).map(hop1 => (hop1, hop2)).orElse(hopsToLCA(n2.maybeParent, hop2 + 1, hopMap)))

  private def hopsToLCA(n1: Node, n2: Node): Option[(Int, Int)] = hopsToLCA(Some(n2), 0, nodeToHopMap(n1))
}
