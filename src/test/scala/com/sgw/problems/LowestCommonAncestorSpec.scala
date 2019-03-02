package com.sgw.problems

import com.sgw.hackerrank.trees.LowestCommonAncestor
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class LowestCommonAncestorSpec extends FlatSpec with Matchers with BeforeAndAfter {
  import com.sgw.hackerrank.trees.LowestCommonAncestor.Node

  "The LowestCommonAncestor" should "find the lowest common ancestor given to values in a binary tree" in {
    val root = Node(
      data = 4,
      left = Some(
        Node(
          data = 2,
          left = Some(
            Node(
              data = 1,
              left = None,
              right = None
            )
          ),
          right = Some(
            Node(
              data = 3,
              left = None,
              right = None
            )
          )
        )
      ),
      right = Some(
        Node(
          data = 7,
          left = Some(
            Node(
              data = 6,
              left = None,
              right = None
            )
          ),
          right = None
        )
      )
    )

    LowestCommonAncestor.lca(Some(root), 1, 7).map(_.data) should be (Some(4))
    LowestCommonAncestor.lca(Some(root), 2, 3).map(_.data) should be (Some(2))
    LowestCommonAncestor.lca(Some(root), 2, 1).map(_.data) should be (Some(2))
//    LowestCommonAncestor.lca(Some(root), 2, 100).map(_.data) should be (None)
  }
}
