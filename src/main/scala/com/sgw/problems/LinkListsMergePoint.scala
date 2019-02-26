package com.sgw.problems

/**
  * Given two singly-linked lists, find the common node (merge point) in the two lists if any.
  *
  * E.g.:
  *
  * x is the common merge point (common node) in the following two link lists whose heads are a and p:
  *
  * [List #1] a--->b--->c
  *                     \
  *                     x--->y--->z--->NULL
  *                     /
  * [List #2]     p--->q
  *
  *
  * From: https://www.hackerrank.com/challenges/find-the-merge-point-of-two-joined-linked-lists/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=linked-lists
  */
object LinkListsMergePoint {

  case class SinglyLinkedListNode(data: Int, maybeNext: Option[SinglyLinkedListNode]) {
    def toSet: Set[SinglyLinkedListNode] = maybeNext.map { nextNode =>
      nextNode.toSet + this
    }.getOrElse {
      Set(this)
    }

    override def toString: String = data.toString + maybeNext.map { nextNode =>
      "->" + nextNode.toString
    }.getOrElse("")
  }

  private def findMergeNodeInSet(
    nodeSet: Set[SinglyLinkedListNode],
    maybeNode: Option[SinglyLinkedListNode]
  ): Option[SinglyLinkedListNode] = maybeNode.flatMap { node =>
    if (nodeSet.contains(node)) {
      Some(node)
    } else {
      findMergeNodeInSet(nodeSet, node.maybeNext)
    }
  }

  def findMergeNode(
    maybeHead1: Option[SinglyLinkedListNode],
    maybeHead2: Option[SinglyLinkedListNode]
  ): Option[SinglyLinkedListNode] = maybeHead1.flatMap { head1Node =>
    findMergeNodeInSet(head1Node.toSet, maybeHead2)
  }
}
