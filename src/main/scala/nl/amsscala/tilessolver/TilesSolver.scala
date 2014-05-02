package nl.amsscala
package tilessolver

object TilesSolver extends App {

  import Direction._

  /** Returns a set of possible chains starting
   *  and ending with a start and ending tile.
   */
  def findChains(tiles: TilesToUse): Set[Chain] = {
    /** Available tiles to combine with. */
    val tilesNotEndingInTheMiddle = tiles.filter(_.end != C)

    /** Class to store intermediate results*/
    case class AssetHandling(val candidates: TilesToUse = tilesNotEndingInTheMiddle, //comparative objects B
                             onHand: TilesToUse = tilesNotEndingInTheMiddle, //Actual unused tiles
                             outHand: Chain = Nil //Actual promising combinations in progress
                             ) {
      /** Function handles the case of an adjacent tile
       *  Meanly by transferring found tile out the onHand List to the outHand List.
       *  If a terminating tile is found a completion is invoked.
       */
      def processFoundTile(previousTile: Tile) =
        {
          val restOnHand = onHand diff List(candidates.head)
          val (assetToTransfer, influenceWalk) = // Test if we found a chain with a center ending tile
            // Invoke a complete found ending sequence by empty list
            if (candidates.head.start == C) (List(candidates.head, previousTile), Nil)
            else (List(previousTile), restOnHand)

          AssetHandling(influenceWalk,
            restOnHand, // explore further without used tile
            // If ending tile save 2 tiles, including the ending one
            assetToTransfer ++ outHand)
        }

      def isNotCompletedTileChain = outHand.isEmpty || (outHand.head.start != C)
    }

    /** The recursive solver*/
    def walk(trail: TilesToUse, //Comparative objects A
             asset: AssetHandling,
             maintainedChains: Set[Chain] /*List of chains so far discovered*/ ): Set[Chain] = {
      if (trail.isEmpty) maintainedChains + asset.outHand // distinct of a set is necessary, don't know why
      else if (asset.candidates.isEmpty) // Try a new walk 
        walk(trail.tail,
          AssetHandling( /*onHand = asset.onHand*/ ),
          if (asset.isNotCompletedTileChain) maintainedChains else maintainedChains + asset.outHand)
      else // Do a matching with each other tile
        walk(trail, AssetHandling(asset.candidates.tail, asset.onHand, asset.outHand),
          maintainedChains ++ (if (trail.head.start isJoinable asset.candidates.head.end) {
            walk( // Start of actual walk parameter list
              List(asset.candidates.head), // explore further with new found tile
              asset.processFoundTile(trail.head),
              maintainedChains) // End of actual walk parameter list
          } else Nil))
    } // def walk(

    walk(tiles.filter(_.end == C).distinct, // Start with ending tiles, one of each
      AssetHandling(candidates = tilesNotEndingInTheMiddle.distinct),
      maintainedChains = Set.empty)
  } // def findChains(

  private val result = findChains(List( // The example on the photo
    Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(W, S),
    Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C)))

  println(
    s"Number of unique chains: ${result.size - 1}, longest: ${result.foldLeft(0)(_ max _.size)} tiles, longest solution(s):")

  // One or more chains could be a valid outcome
  println(result.filter(_.length == result.foldLeft(0)(_ max _.size)).mkString("\n"))
}