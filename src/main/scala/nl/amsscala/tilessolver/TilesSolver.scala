package nl.amsscala
package tilessolver

object TilesSolver extends App {

  /** Compute the placement of tiles in a grid. Every tile has a direction, so
   *  the direction after each tile is known. After a tile a step is made in one
   *  of the 4 directions, this result in a increment/decrement in x or either y.
   */
  def placeTiles(chain: Chain) =
    chain.tail.scanLeft[((Int, Int), Tile), List[((Int, Int), Tile)]](((0, 0), chain.head)) {
      (resultingTuple, currentTile) => (resultingTuple._2.end.step(resultingTuple._1), currentTile)
    }

  /** Compute the extremes, Least Top Left and the Most Bottom Right in one go */
  def computeExtremes(toDraw: Map[(Int, Int), Tile]) =
    toDraw.keys.tail.foldLeft[((Int, Int), (Int, Int))]((toDraw.keys.head, toDraw.keys.head)) {
      (a, tileCoord) =>
        ((a._1._1 min tileCoord._1, a._1._2 min tileCoord._2), // The LTL part of resulting tuple
          (a._2._1 max tileCoord._1, a._2._2 max tileCoord._2)) // The MBR part
    }

  import Directions.{ C, N, E, S, W }

  /** Returns a set of possible chains starting
   *  and ending with a start and ending tile.
   */
  def findChains(tiles: TilesToUse): Set[Chain] = {
    /** Available tiles to combine with. */
    val tilesNotEndingInTheMiddle = tiles.filter(_.end != C)

    /** Intermediate results to store and handle
     *
     *  @constructor	Create a store of 3 List of Tiles
     *  @param	candidates: TilesToUse	Comparative objects B
     *  @param	onHand: TilesToUse Actual unused tiles
     *  @param	outHand: Chain	Actual promising combinations in progress
     */
    case class AssetHandling(val candidates: TilesToUse = tilesNotEndingInTheMiddle,
                             onHand: TilesToUse = tilesNotEndingInTheMiddle,
                             outHand: Chain = Nil) {
      /** Test if the chain is complete */
      def isCompletedTileChain = !outHand.isEmpty && (outHand.head.start == C)

      /** Function handles the case of an adjacent tile
       *  Meanly by transferring found tile out the onHand List to the outHand List.
       *  If a terminating tile is found a completion is invoked.
       */
      def processFoundTile(previousTile: Tile) =
        {
          val restOnHand = onHand diff List(candidates.head)
          val (assetToTransfer, influenceWalk) = // Test if we found a chain with a center ending tile
            // Invoke a complete found ending sequence by empty list if middle tile is found
            if (candidates.head.start == C) (List(candidates.head, previousTile), Nil)
            else (List(previousTile), restOnHand)

          AssetHandling(influenceWalk,
            restOnHand, // explore further without the used tile
            // If ending tile save 2 tiles, including the ending one
            assetToTransfer ++ outHand)
        }
      // Add conditional a Set with the List of build chain
      def transferLastFoundChain(mainChains: Set[Chain]) =
        if (isCompletedTileChain) mainChains + outHand else mainChains
    } // class AssetHandling

    /** The recursive solver*/
    def walk(trail: TilesToUse, //Comparative objects A
             asset: AssetHandling,
             maintainedChains: Set[Chain] /*List of chains so far discovered*/ ): Set[Chain] = {
      // If list is done return result otherwise continue with list
      if (trail.isEmpty) asset.transferLastFoundChain(maintainedChains) // distinct of a set is necessary, don't know why
      else if (asset.candidates.isEmpty) // Try a new walk 
        walk(trail.tail,
          AssetHandling( /*onHand = asset.onHand*/ ),
          asset.transferLastFoundChain(maintainedChains))
      else // Do a matching with each other tile
        walk(trail, AssetHandling(asset.candidates.tail, asset.onHand, asset.outHand), maintainedChains
          ++ (if (trail.head.start isJoinable asset.candidates.head.end) // explore further with new found tile
            walk(List(asset.candidates.head), asset.processFoundTile(trail.head), maintainedChains)
          else Nil))
    } // def walk(

    walk(tiles.filter(_.end == C).distinct, // Start with ending tiles, one of each
      AssetHandling(candidates = tilesNotEndingInTheMiddle.distinct),
      maintainedChains = Set.empty)
  } // def findChains(

  val fabioPhoto =
    List(Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(W, S),
      Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C))

  println(s"Given\n$fabioPhoto")

  private val solution = findChains(fabioPhoto) // The example in the read.me

  private val longestLen = solution.foldLeft(0)(_ max _.size)

  println(
    s"Number of unique chains: ${solution.size}, longest: $longestLen tiles, longest solution(s):")

  // One or more chains could be a valid outcome
  println(solution.filter(_.length >= longestLen).mkString("\n"))
}