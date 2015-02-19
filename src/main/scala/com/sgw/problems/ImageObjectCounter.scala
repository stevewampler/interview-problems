package com.sgw.problems

/**
 * Counts the number of black objects in an otherwise white image.
 *
 * Approach: Scan the image rows for black pixels. When you encounter one, recursively search the pixels around the found pixel for other black pixels.
 *
 * In this case, a "black" pixel has a value of 1, and white pixels have a value of 0. Deal with it.
 */
object ImageObjectCounter {
  private val WHITE = 0
  private val BLACK = 1
  private val NOT_VISITED = 0
  private val VISITED = 1

  // row and column offsets around the selected pixel
  private val OFFSETS = List( (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1) )

  def countObjects(image: Array[Array[Int]]): Int = {
    val nRows = image.size

    if (nRows == 0) return 0

    val nCols = image(0).size

    if (nCols == 0) return 0

    // object counter
    var count = 0

    // use a buffer of the same size to keep track of the pixels we’ve already visited
    val buffer = Array.ofDim[Int](nRows, nCols)

    // scan the image for objects
    (0 until nRows).foreach(row => {
      // scan the current row for a black pixel
      (0 until nCols).foreach(col => {
        // if the pixel is black (not zero) and we haven’t already visited it ...
        if (image(row)(col) == BLACK && buffer(row)(col) == NOT_VISITED) {
          // bump the object count
          count = count + 1

          // search around the pixel for all of the connecting black pixels
          findObject(image, row, col, buffer)
        }
      })
    })

    // return the object count
    count
  }

  // This method finds all of the black pixels connected to the pixel at the specified row and column.
  private def findObject(image: Array[Array[Int]], row: Int, col: Int, buffer: Array[Array[Int]]): Unit = {
    // if the row or column number is outside the image, or the pixel has already been visited, or the pixel is white, bail
    if (row < 0 || row >= image.size || col < 0 || col >= image(row).size || buffer(row)(col) == VISITED || image(row)(col) == WHITE) return

    // mark the pixel as visited
    buffer(row)(col) = VISITED

    // loop over the pixel offsets to check the neighboring pixels
    OFFSETS.foreach {
      case (rowOffset, colOffset) => findObject(image, row + rowOffset, col + colOffset, buffer)
    }
  }
}
