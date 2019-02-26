package com.sgw.problems

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class LinkListsMergePointSpec extends FlatSpec with Matchers with BeforeAndAfter {
  import LinkListsMergePoint.SinglyLinkedListNode

  "The LinkListsMergePoint" should "find the merge point in two link lists that contain a common node" in {
    val commonNode = SinglyLinkedListNode(
      data = 3,
      maybeNext = Some(
        SinglyLinkedListNode(
          data = 4,
          maybeNext = Some(
            SinglyLinkedListNode(
              data = 5,
              maybeNext = None
            )
          )
        )
      )
    )

    val headNode1 = SinglyLinkedListNode(
      data = 1,
      maybeNext = Some(
        SinglyLinkedListNode(
          data = 2,
          maybeNext = Some(
            commonNode
          )
        )
      )
    )

    val headNode2 = SinglyLinkedListNode(
      data = 10,
      maybeNext = Some(
        SinglyLinkedListNode(
          data = 20,
          maybeNext = Some(
            commonNode
          )
        )
      )
    )

    println(headNode1.toString)
    println(headNode2.toString)

    LinkListsMergePoint.findMergeNode(Some(headNode1), Some(headNode2)) should be (Some(commonNode))
  }
}
