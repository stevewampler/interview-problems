package com.sgw.problems

/**
 * Similar to WaterWall, but instead of a wall that can hold water (in 2D), imagine a fountain that can hold water
 * in 3D. The fountain is made of blocks laid out in a 2D matrix (as viewed from the top of the fountain).
 *
 * For example:
 *
 * 3 2 0 1
 * 3 0 0 2
 * 3 2 0 1
 * 2 2 2 2
 *
 * Can't hold any water because the side of the fountain is open.
 *
 * 3 2 1 1
 * 3 0 0 2
 * 3 2 0 1
 * 2 2 2 2
 *
 * Could hold 3 units of water.
 *
 * Water can not leak diagonally.
 *
 * Approach:
 *
 * Start with the bottom layer of bricks and work upward. For each layer, calculate the volume of water
 * the layer can hold by turning the layer into an matrix of true and false values; where true indicates a block exists
 * on the layer at a specific row and column index and false indicates no block exists.
 *
 * Now scan each layer looking for cavities (connected values of false values in a layer's matrix,
 * similar to the "ImageObjectCounter" problem). If cavity is connected to the edge of the layer, the cavity can't hold
 * any water; otherwise it can, and the size of the cavity indicate how much water the cavity can hold.
 *
 * For example, this fountain:
 *
 * 3 2 0 1
 * 3 0 0 2
 * 3 2 0 1
 * 2 2 2 2
 *
 * has a bottom layer that translates to the following layer matrix (where T means the layer contains a block at
 * the specified location and F indicated a the layer has a cavity at the specified location.
 *
 * T T F T
 * T F F T
 * T T F T
 * T T T T
 *
 * which yields the cavity:
 *
 *     F
 *   F F
 *     F
 *
 * But since the cavity is connected to the edge of the fountain (i.e. it has a leak),
 * it can't hold any water, so the fountain's volume is zero.
 *
 * This fountain:
 *
 * 3 2 1 1
 * 3 0 0 2
 * 3 2 0 1
 * 2 2 2 2
 *
 * has a bottom later that translates to the following layer matrix:
 *
 * T T T T
 * T F F T
 * T T F T
 * T T T T
 *
 * which yields the following cavity:
 *
 *
 *   F F
 *     F
 *
 * which is not connected to the edge of the fountain and can therefore hold 3 units of water.
 *
 * The 2nd layer translates to the following layer matrix:
 *
 * T T F F
 * T F F T
 * T T F F
 * T T T T
 *
 * which yields the following cavity:
 *
 *     F F
 *   F F
 *     F F
 *
 * Which can't hold any water, because the cavity is connected to the outer edge of the fountain.
 *
 * Likewise, the top layer translates to the following layer matrix:
 *
 * T F F F
 * T F F F
 * T F F F
 * F F F F
 *
 * with the following cavity:
 *
 *   F F F
 *   F F F
 *   F F F
 * F F F F
 *
 * which also can't hold any water.
 *
 * So the total volume of the fountain is 3 + 0 + 0 = 3.
 *
 * @param fountain a 2D matrix of integers. Each integer, at a specific row and column within the matrix,
 *                 represents the height of the fountain, measured in "blocks", at that location.
 */
case class WaterFountain(fountain: Array[Array[Int]]) {
  /**
   * The number of layers of blocks within the fountain.
   */
  lazy val layerCount = fountain.map(row => row.max).max

  /**
   * The number of rows within the fountain.
   */
  lazy val rowCount = fountain.size

  /**
   * The number of columns within the fountain.
   */
  lazy val colCount = fountain.headOption.map(row => row.size).getOrElse(0)

  /**
   * An array of boolean matrices where each matrix represents
   * a layer of blocks within the fountain. The boolean value
   * indicates that a block either exists (true) or not (false)
   * in the layer at the specified row and column.
   */
  lazy val layers = (0 until layerCount).map(layerIndex => fountain.map(row => row.map(height => height > layerIndex)))

  // row and column offsets around a block within a layer
  // no diagonal offsets are provided, because the fountain can not leak water diagonally
  private val OFFSETS = List( (0, -1), (-1, 0), (0, 1), (1, 0) )

  private def isOnEdge(rowIndex: Int, colIndex: Int, rowCount: Int, colCount: Int): Boolean =
    rowIndex == 0 || colIndex == 0 || rowIndex == (rowCount - 1) || colIndex == (colCount - 1)

  private def calcCavityVolume(
    layer: Array[Array[Boolean]],
    rowIndex: Int,
    colIndex: Int,
    visited: Array[Array[Boolean]],
    rowCount: Int,
    colCount: Int
  ): Int = {
    def go(rowIndex: Int, colIndex: Int, vol: Int = 0, leak: Boolean = false): (Int, Boolean) = {
      // if the row or column index is out of bounds or we've already visited the block ...
      if (rowIndex < 0 || rowIndex >= rowCount || colIndex < 0 || colIndex >= colCount || visited(rowIndex)(colIndex)) {
        // we're done
        return (vol, leak)
      }

      // mark the block as visited
      visited(rowIndex)(colIndex) = true

      // if we've found a block instead of a cavity ...
      if (layer(rowIndex)(colIndex)) {
        // we're done
        return (vol, leak)
      }

      // otherwise, we've found part of a cavity
      val newVol = vol + 1

      // see if the cavity is on an edge of the fountain (and would therefore leak)
      val newLeak = leak || isOnEdge(rowIndex, colIndex, rowCount, colCount)

      OFFSETS.foldLeft((newVol, newLeak)) {
        case ((vol2, leak2), (rowOffset, colOffset)) => go(rowIndex + rowOffset, colIndex + colOffset, vol2, leak2)
      }
    }

    // calculate the cavity's volume and whether or not it would leak
    val (vol3, leak3) = go(rowIndex, colIndex)

    // if the cavity has a leak, it won't hold any water; otherwise it will
    if (leak3) 0 else vol3
  }

  /**
   * Returns the volume of all the non-leaky cavities within a single layer of the fountain's blocks.
   * A leaky cavity is one that's open to the edge of the fountain.
   *
   * @param layer the layer
   */
  private def calcLayerVolume(layer: Array[Array[Boolean]]): Int = {
    // mutable 2D array of booleans used to keep track of the layer's blocks that we've already visited
    val visited = Array.fill[Boolean](rowCount, colCount)(false)

    // visit each of the layer's blocks to find cavities and calculate their total (non-leaky) volume
    (0 until rowCount).foldLeft(0) {
      case (vol1, rowIndex) => (0 until colCount).foldLeft(vol1) {
        case (vol2, colIndex) => vol2 + calcCavityVolume(layer, rowIndex, colIndex, visited, rowCount, colCount)
      }
    }
  }

  /**
   * The volume of water this fountain can hold.
   */
  lazy val volume: Int = layers.foldLeft(0) {
    case (vol, layer) => vol + calcLayerVolume(layer)
  }
}
