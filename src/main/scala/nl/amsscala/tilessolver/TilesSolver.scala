package nl.amsscala
package tilessolver

/** Core code or Model. */
object TilesSolver {
  type TilesToUse = Chain

  import Directions.{C, E, N, S, W}

  /** Returns a set of possible chains starting and ending with a start and ending tile.
    * @param	tiles: TilesToUse List of tiles which have to combined.
    * @return  Set of possible chains starting and ending with a start and ending tile
    */
  def findChains(tiles: TilesToUse): Set[Chain] = {
    /** Available tiles to combine with. */
    val tilesNotEndingInTheMiddle = tiles.filterNot(_.end == C)
    val distinctTilesNotEndingInTheMiddle = tilesNotEndingInTheMiddle.distinct

    /** Intermediate results to store and handle
      *
      * @constructor	Create a store of 3 Lists of Tiles.
      * @param	candidates: TilesToUse	Comparative objects B.
      * @param	tilesLeft: TilesToUse Actual unused tiles.
      * @param	assembledPath: Chain	Actual promising combinations in progress.
      */
    class AssetHandling(val tilesLeft: TilesToUse = tilesNotEndingInTheMiddle,
                        val candidates: TilesToUse = distinctTilesNotEndingInTheMiddle,
                        val assembledPath: Chain = Nil) {
      /** Test if the chain is complete */
      def isCompletedTileChain = assembledPath.nonEmpty && (assembledPath.head.start == C)

      /** Function handles the case of an adjacent tile
        * Meanly by transferring found tile out the tilesLeft List to the assembledPath List.
        * If a terminating tile is found a completion is invoked.
        */
      def processFoundTile(previousTile: Tile) = {
        val restOnHand = tilesLeft diff List(candidates.head)
        val (assetToTransfer, influenceWalk) = // Test if we found a chain with a center ending tile
        // Invoke a complete found ending sequence by empty list if middle tile is found
          if (candidates.head.start == C) (Seq(candidates.head, previousTile), Nil)
          else (Seq(previousTile), restOnHand)

        new AssetHandling(restOnHand, // explore further without the used tile
          influenceWalk,
          // If ending tile save 2 tiles, including the ending one
          assetToTransfer ++ assembledPath)
      }

      // Add conditional a Set with the List of build chain
      def transferLastFoundChain(mainChains: Set[Chain]) =
        if (isCompletedTileChain) mainChains + assembledPath else mainChains
    } // class AssetHandling

    /** The recursive solver */
    def walk(trail: TilesToUse, //Comparative objects A
             asset: AssetHandling,
             maintainedChains: Set[Chain] /*List of chains so far discovered*/): Set[Chain] = {
      // If list is done return result otherwise continue with list
      if (trail.isEmpty) asset.transferLastFoundChain(maintainedChains) // Finished
      else if (asset.candidates.isEmpty) // Add last walked Chain and try a new walk
        walk(trail.tail, new AssetHandling(), asset.transferLastFoundChain(maintainedChains))
      else // Do a matching with each other tile
      // Split up in an unknown path with skipping current match and a path with a match
        walk(trail,
          new AssetHandling(asset.tilesLeft, asset.candidates.tail, asset.assembledPath),
          maintainedChains ++
            (if (trail.head.start isJoinable asset.candidates.head.end) // explore further with new found tile
              walk(List(asset.candidates.head), asset.processFoundTile(trail.head), maintainedChains)
            else Nil)
        )
    } // def walk(

    walk(trail = tiles.filter(_.end == C).distinct, // Start with ending tiles, one of each
      asset = new AssetHandling(),
      maintainedChains = Set.empty)
  } // def findChains(

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
          (layedTile._2._1.whereNext(layedTile._1), (scannedTile, layedTile._2._2 + 1))
      }

  /** Remove the solutions with double tiles on one place
    * by comparing the raw length of each path with the real length without double positions.
    * Done by the keySET of a map where no double keys are possible.
    */
  def filterRealSolutions(rawSolutions: Set[Chain], unFiltered: Boolean) =
    rawSolutions.filter(p => unFiltered || p.lengthCompare(virtualTilesLayouter(p).toMap.size) <= 0)

  /** Find tile positions which are overlaid */
  def findOverlayedPositions =
    (_: Seq[LayedTile]).groupBy(_._1).filter { case (coord, grouplist) => grouplist.lengthCompare(1) > 0}.keySet

  /** Compute the extremes, Least Top Left and the Most Bottom Right in one go */
  def calculateExtremes(toDraw: Map[(Int, Int), (Tile, Int)]): ((Int, Int), (Int, Int)) =
    toDraw.keys.tail.foldLeft[((Int, Int), (Int, Int))]((toDraw.keys.head, toDraw.keys.head)) {
      (a, tileCoord) =>
        ((a._1._1 min tileCoord._1, a._1._2 min tileCoord._2), // The LTL part of resulting tuple
          (a._2._1 max tileCoord._1, a._2._2 max tileCoord._2)) // The MBR part
    }

  /** Original given tiles as published on a photo. */
  def fabioPhoto = List(Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(W, S),
      Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C))

  def modifiedExample = List(Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(E, S), // Modified example
    Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C))

  def missingsTileOverlap = List(Tile(C, E), Tile(N, S), Tile(S, N), Tile(W, S), Tile(N, W), Tile(E, N), Tile(S, C))

  def crazyExample = modifiedExample ++ List(Tile(C, N), Tile(W, N))

} // object TilesSolver