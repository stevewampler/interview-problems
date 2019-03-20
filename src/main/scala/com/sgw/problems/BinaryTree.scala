package com.sgw.problems

/**
  * An immutable binary tree node.
  *
  * @param value the node's value
  * @param maybeLeft the node's optional left node
  * @param maybeRight the node's optional right node
  * @tparam V the node value's type
  *
  * Issues: This implementation relies heavily on non-tail recursive functions. Next step is to either make
  * them tail recursive (if possible), or re-implement them with stacks and queues.
  */
case class BinaryTreeNode[V](
  value: V,
  maybeLeft: Option[BinaryTreeNode[V]] = None,
  maybeRight: Option[BinaryTreeNode[V]] = None
)(
  implicit ordering: Ordering[V]
) {
  def size: Int = toListInOrder.size

  def min: BinaryTreeNode[V] = maybeLeft.map(_.min).getOrElse(this)

  def max: BinaryTreeNode[V] = maybeRight.map(_.max).getOrElse(this)

  /**
    * Inserts the specified node into this node's tree.
    *
    * @param node the node ot insert
    *
    * @return a copy of this node with the inserted node as one of its children
    */
  def insert(node: BinaryTreeNode[V]): BinaryTreeNode[V] = {
    if (ordering.compare(node.value, value) < 0) {
      copy(
        maybeLeft = maybeLeft.map { left =>
          left.insert(node)
        } orElse {
          Some(node)
        }
      )
    } else {
      copy(
        maybeRight = maybeRight.map { right =>
          right.insert(node)
        } orElse {
          Some(node)
        }
      )
    }
  }

  /**
    * Deleted the node containing the target value if one is found.
    *
    * @param targetValue the target value
    *
    * @return an optional edited copy of this node. If the option is empty, then this node was the one
    *         deleted and this node had no children. If the option is not empty, then the returned node
    *         is a copy of this node with the target node removed (if found).
    */
  def delete(targetValue: V): Option[BinaryTreeNode[V]] = {
    val comparison = ordering.compare(targetValue, value)

    if (comparison == 0) {
      // this is the node that needs to be deleted
      maybeLeft.map { left =>
        maybeRight.map { right =>
          left.insert(right)
        }.getOrElse(left)
      } orElse maybeRight
    } else if (comparison < 0) {
      maybeLeft.map { left =>
        copy(
          maybeLeft = left.delete(targetValue)
        )
      } orElse {
        Some(this)
      }
    } else {
      maybeRight.map { right =>
        copy(
          maybeRight = right.delete(targetValue)
        )
      } orElse {
        Some(this)
      }
    }
  }

  /**
    * Searches this node and its children for the node that contains the specified
    * target value and returns the path (a list of nodes) from
    * the target node to this if a target node was found; otherwise this method returns an empty path.
    *
    * @param targetValue the value to be found
    * @param path the path from this node's parent to the root of the tree
    *
    * @return the path (list of nodes) from the target node, if found, to the root of the tree or an empty
    *         path if the target was not found.
    */
  def search(
    targetValue: V,
    path: List[BinaryTreeNode[V]] = List.empty[BinaryTreeNode[V]]
  ): Seq[BinaryTreeNode[V]] = {
    val comparison = ordering.compare(targetValue, value)

    if (comparison == 0) {
      this :: path
    } else if (comparison < 0) {
      maybeLeft.map(_.search(targetValue, this :: path)).getOrElse(Seq.empty[BinaryTreeNode[V]])
    } else {
      maybeRight.map(_.search(targetValue, this :: path)).getOrElse(Seq.empty[BinaryTreeNode[V]])
    }
  }

  /**
    * Returns a list this node and its children pre order
    */
  def toListPreOrder: List[BinaryTreeNode[V]] = {
    val leftList = maybeLeft.map(_.toListPreOrder).getOrElse(List.empty[BinaryTreeNode[V]])
    val rightList = maybeRight.map(_.toListPreOrder).getOrElse(List.empty[BinaryTreeNode[V]])

    this :: (leftList ++ rightList)
  }

  /**
    * Returns a list this node and its children in order
    */
  def toListInOrder: List[BinaryTreeNode[V]] = {
    val leftList = maybeLeft.map(_.toListInOrder).getOrElse(List.empty[BinaryTreeNode[V]])
    val rightList = maybeRight.map(_.toListInOrder).getOrElse(List.empty[BinaryTreeNode[V]])

    leftList ++ (this :: rightList)
  }

  /**
    * Returns a list this node and its children post order
    */
  def toListPostOrder: List[BinaryTreeNode[V]] = {
    val leftList = maybeLeft.map(_.toListPostOrder).getOrElse(List.empty[BinaryTreeNode[V]])
    val rightList = maybeRight.map(_.toListPostOrder).getOrElse(List.empty[BinaryTreeNode[V]])

    (leftList ++ rightList) :+ this
  }
}

/**
  * An immutable BinaryTree.
  *
  * @param maybeRoot an optional root node for this tree.
  * @param ordering the order for the values of type V
  * @tparam V the type of the values held in this tree
  *
  * TODO: make this collection properly handle variants.
  */
case class BinaryTree[V](
  maybeRoot: Option[BinaryTreeNode[V]] = None
)(
  implicit ordering: Ordering[V]
) {
  def isEmpty: Boolean = maybeRoot.isEmpty
  def nonEmpty: Boolean = maybeRoot.nonEmpty

  def size: Int = maybeRoot.map(_.size).getOrElse(0)

  def +(value: V): BinaryTree[V] = insert(value)

  def insert(value: V): BinaryTree[V] = copy(
    maybeRoot = maybeRoot.map { root =>
      root.insert(BinaryTreeNode(value))
    } orElse Some(BinaryTreeNode[V](value))
  )

  def -(value: V): BinaryTree[V] = delete(value)

  def delete(value: V): BinaryTree[V] = copy(
    maybeRoot = maybeRoot.flatMap(_.delete(value))
  )

  def search(value: V): Seq[V] = maybeRoot.map { root =>
    root.search(value).map(_.value)
  }.getOrElse {
    Seq.empty[V]
  }

  def min: Option[V] = maybeRoot.map(_.min).map(_.value)
  def max: Option[V] = maybeRoot.map(_.max).map(_.value)

  def previous(value: V): Option[V] = maybeRoot.map { root =>
    root.search(value)
  }.flatMap { path =>
    path.headOption.flatMap { targetNode =>
      targetNode.maybeLeft.map(_.max) orElse {
        // find the ancestor whose right node is in the path
        path.zip(path.tail).dropWhile { case (child, parent) =>
          parent.maybeLeft.contains(child)
        }.headOption.map { case (_, parent) =>
          parent
        }
      }
    }.map(_.value)
  }

  def next(value: V): Option[V] = maybeRoot.map { root =>
    root.search(value)
  }.flatMap { path =>
    path.headOption.flatMap { targetNode =>
      targetNode.maybeRight.map(_.min) orElse {
        // find the ancestor whose left node is in the path
        path.zip(path.tail).dropWhile { case (child, parent) =>
          parent.maybeRight.contains(child)
        }.headOption.map { case (_, parent) =>
          parent
        }
      }
    }.map(_.value)
  }

  def toListPreOrder: List[V] = maybeRoot.map(_.toListPreOrder).getOrElse(List.empty[BinaryTreeNode[V]]).map(_.value)
  def toListInOrder: List[V] = maybeRoot.map(_.toListInOrder).getOrElse(List.empty[BinaryTreeNode[V]]).map(_.value)
  def toListPostOrder: List[V] = maybeRoot.map(_.toListPostOrder).getOrElse(List.empty[BinaryTreeNode[V]]).map(_.value)
}

object BinaryTree {
  def empty[V](
    implicit ordering: Ordering[V]
  ): BinaryTree[V] = BinaryTree[V]()

  def main(args: Array[String]): Unit = {
    // creates and tests the following binary tree:
    //
    //     15
    //    / \
    //   10  16
    //  / \   \
    // 5   12  17
    //     / \
    //   11   13
    //

    assert(BinaryTree.empty[Int].isEmpty)
    assert(!BinaryTree.empty[Int].nonEmpty)

    val tree = BinaryTree.empty[Int] + 15 + 10 + 16 + 5 + 12 + 11 + 17 + 13

    assert(!tree.isEmpty)
    assert(tree.nonEmpty)
    assert(tree.size == 8)

    val preorder  = tree.toListPreOrder.mkString(",")
    val inorder   = tree.toListInOrder.mkString(",")
    val postorder = tree.toListPostOrder.mkString(",")

//    println(tree)

//    println(s"preorder  = $preorder")
//    println(s"inorder   = $inorder")
//    println(s"postorder = $postorder")

    assert(preorder == "15,10,5,12,11,13,16,17")
    assert(inorder == "5,10,11,12,13,15,16,17")
    assert(postorder == "5,11,13,12,10,17,16,15")

    assert(tree.min.contains(5))
    assert(tree.max.contains(17))

    assert(tree.search(5).headOption.contains(5))
    assert(tree.search(15).headOption.contains(15))
    assert(tree.search(12).headOption.contains(12))
    assert(tree.search(100).isEmpty)

    assert(tree.next( 5).contains(10))
    assert(tree.next(10).contains(11))
    assert(tree.next(11).contains(12))
    assert(tree.next(12).contains(13))
    assert(tree.next(13).contains(15))
    assert(tree.next(15).contains(16))
    assert(tree.next(16).contains(17))
    assert(tree.next(17).isEmpty)
    assert(tree.next(100).isEmpty)

    assert(tree.previous(17).contains(16))
    assert(tree.previous(16).contains(15))
    assert(tree.previous(15).contains(13))
    assert(tree.previous(13).contains(12))
    assert(tree.previous(12).contains(11))
    assert(tree.previous(11).contains(10))
    assert(tree.previous(10).contains(5))
    assert(tree.previous( 5).isEmpty)
    assert(tree.previous(100).isEmpty)

    println(tree)
    println(tree - 12)

    assert((tree -  5).toListInOrder.mkString(",") == "10,11,12,13,15,16,17")
    assert((tree - 10).toListInOrder.mkString(",") == "5,11,12,13,15,16,17")
    assert((tree - 11).toListInOrder.mkString(",") == "5,10,12,13,15,16,17")
    assert((tree - 12).toListInOrder.mkString(",") == "5,10,11,13,15,16,17")
    assert((tree - 13).toListInOrder.mkString(",") == "5,10,11,12,15,16,17")
    assert((tree - 15).toListInOrder.mkString(",") == "5,10,11,12,13,16,17")
    assert((tree - 16).toListInOrder.mkString(",") == "5,10,11,12,13,15,17")
    assert((tree - 17).toListInOrder.mkString(",") == "5,10,11,12,13,15,16")
    assert((tree - 100).toListInOrder.mkString(",") == "5,10,11,12,13,15,16,17")
  }
}
