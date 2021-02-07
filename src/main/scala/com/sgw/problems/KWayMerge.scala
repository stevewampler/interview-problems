package com.sgw.problems

import com.sgw.utils.{Heapsort, MaxHeap}

import scala.annotation.tailrec

/**
  * Merges k lists of sorted integers into a single list of sorted integers.
  */
object KWayMerge extends App {

  // brute force: flatten the lists and resort it
  // O(n log n)
  def merge1(listOfLists: List[List[Int]], list: List[Int] = List()): List[Int] = {
    listOfLists.flatten.sorted
  }

  // brute force: flatten the lists and resort it using a max-heap
  // O(n log n)
  def merge2(listOfLists: List[List[Int]], list: List[Int] = List()): List[Int] = {
    val arr = listOfLists.flatten.toArray

    val maxHeap = MaxHeap(arr)

    maxHeap.toList.reverse
  }

  // brute force: find the minimum list
  // O(nk)
  @tailrec
  def merge3(listOfLists: List[List[Int]], list: List[Int] = List()): List[Int] = {

    if (listOfLists.isEmpty) return list

    val minList = listOfLists.minBy { list => list.head }

    val minValue = minList.head

    val newMinList = minList.tail

    val newListOfLists = listOfLists.map { list =>
      if (list == minList) {
        newMinList
      } else {
        list
      }
    }.filter { list =>
      list.nonEmpty
    }

    merge3(newListOfLists, minValue :: list)
  }

  // use a heap to find the min list in O(log k) time and do it O(n) times so O(n log k)
  def merge4(listOfLists: List[List[Int]]): List[Int] = {

    implicit val listOrdering = new Ordering[List[Int]] {
      override def compare(list1: List[Int], list2: List[Int]): Int = {
        list2.head - list1.head
      }
    }

    val arrayOfLists: Array[List[Int]] = listOfLists.toArray

    @tailrec
    def go(arrayOfLists: Array[List[Int]], list: List[Int] = List.empty[Int]): List[Int] = {

      if (arrayOfLists.isEmpty) return list

      // this mutates the arrayOfLists, which is okay because it's not exposed outside of the merge4 function
      // O(log k)
      Heapsort.heapify(arrayOfLists)(listOrdering)

      val minList = arrayOfLists.head

      val minValue = minList.head

      val newMinList = minList.tail

      val newArrayOfLists = if (newMinList.isEmpty) {
        arrayOfLists.tail
      } else {
        arrayOfLists(0) = newMinList
        arrayOfLists
      }

      go(newArrayOfLists, minValue :: list)
    }

    go(arrayOfLists)
  }

  val listOfLists = List(
    List(1,5,7,8,10),
    List(2,4,5,7,20),
    List(3,4,6,8,20,25)
  )

  val expected = listOfLists.flatten.sorted

  println(expected)

  {
    val actual = merge1(listOfLists)

    println(actual)

    assert(actual == expected)
  }

  {
    val actual = merge2(listOfLists)

    println(actual)

    assert(actual == expected)
  }

  {
    val actual = merge3(listOfLists).reverse

    println(actual)

    assert(actual == expected)
  }

  {
    val actual = merge4(listOfLists).reverse

    println(actual)

    assert(actual == expected)
  }
}
