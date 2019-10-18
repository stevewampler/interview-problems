package com.sgw.utils

import scala.math.Ordering

// See https://en.wikipedia.org/wiki/Heapsort
object Heapsort {
  def parent(i: Int) = Math.floor((i - 1.0) / 2.0).toInt
  def leftChild(i: Int)  = 2 * i + 1
  def rightChild(i: Int) = 2 * i + 2

  def swap[T](arr: Array[T], i: Int, j: Int): Unit = {
    val tmp = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  }

  /**
   * Sorts an the specified array in place using the heapsort algorithm.
   *
   * @tparam T the type of the elements in the array
   * @param arr an unordered array of type T
   * @param ord an implementation of the Ordering trait for object of type T
   */
  def heapsort[T](arr: Array[T])(implicit ord: Ordering[T]): Array[T] = {
    // build the heap in array 'a' so that largest value is at the root
    heapify(arr, 0)

    // the following loop maintains the invariants that a[0:end] is a heap and every element
    // beyond end is greater than everything before it (so a[end:count] is in sorted order)
    var end = arr.length - 1

    while (end > 0) {
      // a[0] is the root and largest value. The swap moves it in front of the sorted elements.
      swap(arr, end, 0)

      // the heap size is reduced by one
      end = end - 1

      // the swap ruined the heap property, so restore it
      siftDown(arr, 0, 0, end)
    }

    arr
  }

  /**
   * Heapifies the elements of the specified array starting at the 'begin' element.
   *
   * @param arr the array
   * @param begin where, in the array, to start the heapify
   * @param ord an implementation of the Ordering trait for object of type T
   * @tparam T the type of the elements in the array
   */
  def heapify[T](arr: Array[T], begin: Int = 0)(implicit ord: Ordering[T]) = {
    val count = arr.length - begin

    (parent(count - 1) to 0 by -1).foreach(start => siftDown(arr, begin, start, count - 1))
  }

  /**
   * Repair the heap located at the 'begin + start' element of the specified array,
   * assuming the heaps rooted at its children are valid)
   *
   * @tparam T the type of elements in the specified array
   * @param arr the array
   * @param begin the zero-based location of the ultimate root of the heap. In a normal heap-sort, the being
   *              value will always be zero (0).
   * @param start the zero-based location of the first element within the heap.
   * @param end the zero-based location of the last element within the heap.
   */
  def siftDown[T](arr: Array[T], begin: Int, start: Int, end: Int)(implicit ord: Ordering[T]): Unit = {
    var root = start

    while (leftChild(root) <= end) { // while the root has at least one child
      val child = leftChild(root) // left child of root

      var childToSwap = root  // keeps track of child to swap with

      if (ord.lt(arr(begin + childToSwap), arr(begin + child))) {
        childToSwap = child
      }

      // if there is a right child and that child is greater ...
      if ((child + 1) <= end && ord.lt(arr(begin + childToSwap), arr(begin + child + 1))) {
        childToSwap = child + 1
      }

      if (childToSwap == root) {
        // the root holds the largest element. Since we assume the heaps rooted at the
        // children are valid, this means that we are done.
        return
      } else {
        swap(arr, begin + root, begin + childToSwap)

        root = childToSwap // repeat to continue sifting down the child now
      }
    }
  }
}
