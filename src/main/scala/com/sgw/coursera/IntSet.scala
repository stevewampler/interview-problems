package com.sgw.coursera

/**
  * Created by swampler on 8/31/16.
  */
abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  def union(s: IntSet): IntSet

  def forall(p: Int => Boolean): Boolean

  def toList: List[Int]

  override def toString: String = toList.mkString(",")
}

case object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x)
  def union(s: IntSet): IntSet = s
  def forall(p: Int => Boolean): Boolean = true

  def toList: List[Int] = List()
}

case class NonEmpty(elem: Int, left: IntSet = Empty, right: IntSet = Empty) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem)
      left.contains(x)
    else if (x > elem)
      right.contains(x)
    else
      true

  def incl(x: Int): IntSet =
    if (x == elem) this
    else new NonEmpty(
      elem,
      if (x < elem) left.incl(x) else left,
      if (x > elem) right.incl(x) else right
    )

  def union(s: IntSet): IntSet = right.union(left.union(s.incl(elem)))

  def forall(p: Int => Boolean): Boolean = p(elem) && left.forall(p) && right.forall(p)

  def toList: List[Int] = elem :: (left.toList ++ right.toList)
}