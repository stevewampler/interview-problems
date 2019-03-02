package com.sgw.hackerrank.trees

/**
  * You are given pointer to the root of the binary search tree and two values v1 and v2.
  * You need to return the lowest common ancestor (LCA) of v1 and v2 in the binary search tree.
  *
  *     2
  *    / \
  *   1  3
  *     / \
  *    4   5
  *         \
  *          6
  *
  * In the diagram above, the lowest common ancestor of the nodes 4 and 6
  * is the node 3. Node 3 is the lowest node which has nodes 4 and 5 as descendants.
  *
  * Function Description
  *
  * Complete the function lca in the editor below. It should return a pointer to the lowest common ancestor node of the two values given.
  *
  * lca has the following parameters:
  * - root: a pointer to the root node of a binary search tree
  * - v1: a node.data value
  * - v2: a node.data value
  *
  * Input Format
  *
  * The first line contains an integer, n, the number of nodes in the tree.
  * The second line contains n space-separated integers representing node.data values.
  * The third line contains two space-separated integers, v1 and v2.
  *
  * To use the test data, you will have to create the binary search tree yourself.
  * Here on the platform, the tree will be created for you.
  *
  * The tree will contain nodes with data equal to v1 and v2.
  *
  * Output Format
  *
  * Return the a pointer to the node that is the lowest common ancestor of v1 and v2.
  *
  * Sample Input
  *
  * 6
  * 4 2 3 1 7 6
  * 1 7
  *
  *        4
  *      /   \
  *     2     7
  *    / \   /
  *   1   3 6
  *
  * v1 = 1 and v2 = 7.
  *
  * Sample Output
  *
  * [reference to node 4]
  *
  * Explanation
  *
  * LCA of 1 and 7 is 4, the root in this case.
  *
  * From: https://www.hackerrank.com/challenges/binary-search-tree-lowest-common-ancestor/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=trees
  */
object LowestCommonAncestor {

  case class Node(var data: Int, left: Option[Node], right: Option[Node])

  def lca(root: Option[Node], v1: Int, v2: Int): Option[Node] = {
    if (v1 > v2) return lca(root, v2, v1)

    root.flatMap { node =>
      if (node.data == v1 || node.data == v2) { // if the node's value is either v1 or v2 ...
        root // we found the LCA
      } else if (v2 < node.data) { // if both values are on the left side of the node ...
        lca(node.left, v1, v2) // go left
      } else if (node.data < v1) { // if both values are on the right side of the node ...
        lca(node.right, v1, v2) // go right
      } else { // otherwise ...
        root // we found the LCA
      }
    }
  }
}
