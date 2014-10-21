package nl.amsscala
package tilessolver

/** Core code or Model. */
object TilesSolver {
  type TilesToUse = Chain

  import nl.amsscala.tilessolver.Directions.{C, E, N, S, W}

  /** Returns a set of possible chains starting and ending with a start and ending tile.
    * @param	tiles: TilesToUse List of tiles which have to combined.
    * @return  Set of possible chains starting and ending with a start and ending tile
    */
    def findChains(tiles: TilesToUse): Set[Chain] = {

      val candidates = tiles.filter(!_.isEndingTile)

      def ends = tiles.filter(_.isEndingTile).toSet

      def evaluateChain(tilesLeft: TilesToUse, path: Chain): Set[Chain] = {
        def possibleTiles = tilesLeft.filter(path.head.isValidAdjacent).toSet

        possibleTiles.flatMap { tile =>
          if (tile.start == C) Set(tile +: path)
          else evaluateChain(tilesLeft diff List(tile), tile +: path)
        }
      }

      ends.flatMap { endingTile2StartWith => evaluateChain(candidates, Seq(endingTile2StartWith))}
    } // findChains


  /** Filter the longest solutions
   *
   * @param solution All small and long solutions
   * @return  The longest solutions
   */
  def allLongestSolutions(solution : Set[Chain]) = {
    solution.foldLeft(Set.empty[Chain]) { (cumulatedSameLengthLongestChains, chain) =>
      def currentChainLength = cumulatedSameLengthLongestChains.headOption.map(_.length).getOrElse(0)

      val longestSoFar = currentChainLength max chain.length

      (cumulatedSameLengthLongestChains + chain).filter(_.length == longestSoFar)
    }
  }

  /** Remove the solutions with double tiles on one place
    * by comparing the raw length of each path with the real length without double positions.
    * Done by the keySET of a map where no double keys are possible.
    */
  def filterRealSolutions(rawSolutions: Set[Chain], unFiltered: Boolean) =
    rawSolutions.filter(p => unFiltered || p.lengthCompare(virtualTilesLayouter(p).toMap.size) <= 0)

  /** Compute the placement of tiles in a grid. Every tile has a direction, so the direction after each tile is known.
    * After a tile a step is made in one of the 4 directions, this result in a increment/decrement in x or either y.
    *
    * returns ((coordinate),(Tile,n)) list, n is serial number added for later sorting.
    */
  def virtualTilesLayouter(chain: Chain): Seq[LayedTile] =
    if (chain.isEmpty) Nil
    else
      chain.tail.scanLeft[LayedTile, Seq[LayedTile]]((0, 0), (chain.head, 0)) {
        (layedTile, scannedTile) =>
          (layedTile._2._1.whereIsNextLayed(layedTile._1), (scannedTile, layedTile._2._2 + 1))
      }

  /** Find tile positions which are overlaid */
  def findOverlayedPositions =
    (_: Seq[LayedTile]).groupBy(_._1).filter { case (coord, grouplist) => grouplist.lengthCompare(1) > 0}.keySet

  /** Compute the extremes, Least Top Left and the Most Bottom Right in one go */
  def calculateExtremes(toDraw: Map[Coord, (Tile, Int)]): (Coord, Coord) =
    toDraw.keys.tail.foldLeft((toDraw.keys.head, toDraw.keys.head)) {
       (extremeRectangular, tileCoord) =>
        ((extremeRectangular._1._1 min tileCoord._1, extremeRectangular._1._2 min tileCoord._2), // The LTL part of resulting tuple
          (extremeRectangular._2._1 max tileCoord._1, extremeRectangular._2._2 max tileCoord._2)) // The MBR part
    }

  /** Original given tiles as published on a photo. */
  def fabioPhoto = List(Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E),
    Tile(W, S), Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C))

  def missingsTileOverlap = List(Tile(C, E), Tile(N, S), Tile(S, N), Tile(W, S), Tile(N, W), Tile(E, N), Tile(S, C))

  def crazyExample = modifiedExample ++ List(Tile(C, N), Tile(W, N))

  def modifiedExample = List(Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(E, S), // Modified example
    Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C))

} // object TilesSolver