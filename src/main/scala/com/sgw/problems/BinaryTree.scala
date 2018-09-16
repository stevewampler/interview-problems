package com.sgw.problems

object BinaryTree {

  /**
    * A binary tree node.
    *
    * @param value the node's value
    * @param maybeLeft the node's optional left node
    * @param maybeRight the node's optional right node
    * @tparam V the node value's type
    */
  case class Node[V](value: V, maybeLeft: Option[Node[V]] = None, maybeRight: Option[Node[V]] = None) {
    /**
      * Returns a list this node's values, and it's children, pre order
      */
    def toListPreOrder: List[V] = {
      val leftList = maybeLeft.map(_.toListPreOrder).getOrElse(List.empty[V])
      val rightList = maybeRight.map(_.toListPreOrder).getOrElse(List.empty[V])

      value :: (leftList ++ rightList)
    }

    /**
      * Returns a list this node's values, and it's children, in order
      */
    def toListInOrder: List[V] = {
      val leftList = maybeLeft.map(_.toListInOrder).getOrElse(List.empty[V])
      val rightList = maybeRight.map(_.toListInOrder).getOrElse(List.empty[V])

      leftList ++ (value :: rightList)
    }

    /**
      * Returns a list this node's values, and it's children, post order
      */
    def toListPostOrder: List[V] = {
      val leftList = maybeLeft.map(_.toListPostOrder).getOrElse(List.empty[V])
      val rightList = maybeRight.map(_.toListPostOrder).getOrElse(List.empty[V])

      (leftList ++ rightList) :+ value
    }
  }

  def main(args: Array[String]): Unit = {
    // creates the following binary tree:
    //
    //     A
    //    / \
    //   B   C
    //  / \   \
    // D   E   F
    //     /
    //    G
    //
    // and prints out its values
    //
    // * pre order: A,B,D,E,G,C,F
    // * in order: D,B,G,E,A,C,F
    // * post order: D,G,E,B,F,C,A
    //
    val tree =
      Node(
        value = "A",
        maybeLeft = Some(
          Node(
            value = "B",
            maybeLeft = Some(
              Node(
                value = "D"
              )
            ),
            maybeRight = Some(
              Node(
                value = "E",
                maybeLeft = Some(
                  Node(
                    value = "G"
                  )
                )
              )
            )
          )
        ),
        maybeRight = Some(
          Node(
            value = "C",
            maybeRight = Some(
              Node(
                value = "F"
              )
            )
          )
        )
      )

    val preorder  = tree.toListPreOrder.mkString(",")
    val inorder   = tree.toListInOrder.mkString(",")
    val postorder = tree.toListPostOrder.mkString(",")

    assert(preorder == "A,B,D,E,G,C,F")
    assert(inorder == "D,B,G,E,A,C,F")
    assert(postorder == "D,G,E,B,F,C,A")

    println(s" pre order: $preorder")
    println(s"  in order: $inorder")
    println(s"post order: $postorder")
  }
}
