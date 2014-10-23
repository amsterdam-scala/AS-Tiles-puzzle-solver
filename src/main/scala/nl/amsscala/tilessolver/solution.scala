package nl.amsscala
package tilessolver

class Solution(val rawSolution: Set[Chain]) {

  override def toString = rawSolution.toString()

  /** Filter the longest solutions
    *
    * @return  The longest solutions
    */
  def allLongestSolutions(filtered: Boolean = false) = {
    /** Remove the conditonal solutions with double tiles on one place
      * by comparing the raw length of each path with the real length without overlayed positions.
      * Done by the keySET of a Map where multiple identical keys are overwritten.
      */
    def filterRealSolutions =
      rawSolution.filter(p => !filtered || p.lengthCompare(Solution.virtualTilesLayouter(p).toMap.size) <= 0)


    filterRealSolutions.foldLeft(Set.empty[Chain]) { (cumulatedSameLengthLongestChains, chain) =>
      def currentChainLength = cumulatedSameLengthLongestChains.headOption.map(_.size).getOrElse(0)

      val longestSoFar = currentChainLength max chain.size

      (cumulatedSameLengthLongestChains + chain).filter(_.lengthCompare(longestSoFar) >= 0)
    }
  }

} // class Solution

object Solution {
  def apply(tilesToUse: Chain) = new Solution(findChains(tilesToUse))

  /** Returns a set of possible chains starting and ending with a start and ending tile.
    * @param	tilesToUse: TilesToUse List of tiles which have to combined.
    * @return  Set of possible chains starting and ending with a start and ending tile
    */
  private def findChains(tilesToUse: Chain): Set[Chain] = {

    val candidates = tilesToUse.filter(!_.isEndingTile)

    def ends = tilesToUse.filter(_.isEndingTile).toSet

    def evaluateChain(tilesLeft: Chain, path: Chain): Set[Chain] = {
      def possibleTiles = tilesLeft.filter(path.head.isValidAdjacent).toSet

      possibleTiles.flatMap { tile =>
        def extension = tile +: path
        if (tile.isStartTile) Set(extension) else evaluateChain(tilesLeft diff Seq(tile), extension)
      }
    }

    ends.flatMap(endingTile2StartWith => evaluateChain(candidates, Seq(endingTile2StartWith)))
  } // findChains

  def apply(solution: Set[Chain]) = new Solution(solution)

  /** Computes the placement of tiles in a grid. Every tile has a direction, so the place after each tile is known.
    * After a tile a step is made in one of the 4 directions, this result in a increment/decrement in x or either y.
    *
    * ScanLeft cumulates a sequence of intermediate cumulative results using a start value.
    * @return ((coordinate),(Tile,n)) list, n is serial number added for later sorting.
    */
  def virtualTilesLayouter = (_: Chain) match {
    case Nil => Nil
    case chain: Chain => chain.tail.scanLeft[LayedTile, Seq[LayedTile]]((0, 0), (chain.head, 0)) {
      case ((position, (tile, n)), scannedTile) => (tile.whereIsNextLayed(position), (scannedTile, n + 1))
    }
  }

  /** Find tile positions which are overlaid
    * @param _ A list of LayedTiles
    * @return Set of coordinates of where tiles are overlapping
    */
  def findOverlayedPositions =
    (_: Seq[LayedTile]).groupBy(_._1).filter { case (coord, grouplist) => grouplist.lengthCompare(1) > 0}.keySet

}

object Suggestions {

  import Directions.{C, E, N, S, W}

  /** Original given tiles as published on a photo. */
  def fabioPhoto = List(Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E),
    Tile(W, S), Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C))

  def missingsTileOverlap = List(Tile(C, E), Tile(N, S), Tile(S, N), Tile(W, S), Tile(N, W), Tile(E, N), Tile(S, C))

  def crazyExample = modifiedExample ++ List(Tile(C, N), Tile(W, N))

  def modifiedExample = List(Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(E, S), // Modified example
    Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C))

} // object TilesSolver
