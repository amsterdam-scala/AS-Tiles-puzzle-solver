package nl.amsscala
package tilessolver

import scala.annotation.tailrec

/** Tile solver program
 *
 *  Find the longest chain of connected tiles.
 *
 *  Signature is `findChains(tiles: TilesToUse): Set[Chain]`
 *  where `TilesToUse` is an unordered `List[Tile]`
 *  and `Chain` is a sequential `List[Tile]`
 *  so `Set[Chain] = Set[List[Tile]]`
 *  The first element of a `Chain` holds the start tile and
 *  the element the ending tile.
 *
 *  Theoretically the outcome should be:
 *
 *  `Set[List[Chain]]` a.k.a. `Set[List[List[Tile]]]`
 *  because out the leftover tiles eventually
 *  more coexisting chain(s) could be found.
 *  e.g. `Set(List(List(Tile(C,E),Tile(W,C)),
 *                List(Tile(C,E),Tile(W,C))))`
 *
 *  But this has to be left for a future exercise.
 *
 *  @author A'dam Scala Tiles-puzzle-solver team
 */
object TilesSolver extends App {

  /** Enumeration of the connection side of a tile*/
  object Direction extends Enumeration {
    case class Direction() extends Val {
      /** Returns allowed tile side chain-joint*/
      def allowedAdjacent =
        this match {
          case N => S
          case E => W
          case S => N
          case W => E
        }

      /** Test if the sides of titles pair could be adjacent.
       *  The function return true if the ending side meets a legal terminating side.
       */
      def isJoinable(adjacent: Direction) = (this != C) && adjacent == allowedAdjacent
    }
    /** Side names of Tiles */
    val C, N, E, S, W = Direction() // Center, North, East, South ...
  } // object Direction

  import Direction._

  /** Descriptor for a tile direction indicated with a arrow
   *  @param	start The from or incoming of tile (tail of arrow)
   *  @param	end	The to or outgoing side of tile (arrowhead)
   *  @throws	java.lang.IllegalArgumentException If start and end are the same.
   */
  case class Tile(val start: Direction, val end: Direction) {
    require(start != end, s"Not a proper tile definition, given $start, $end are the same.")
  }

  type Chain = List[Tile]
  type TilesToUse = Chain

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

    walk(trail = tiles.filter(_.end == C).distinct, // Start with ending tiles, one of each
      AssetHandling(candidates = tilesNotEndingInTheMiddle.distinct),
      maintainedChains = Set.empty)
  } // def findChains(

  val result = findChains(List( // The example on the photo
    Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(W, S),
    Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C)))

  println(s"Number of unique chains: ${result.size - 1}, solution(s):")
  println(result.filter(_.length == result.maxBy(_.length).length).mkString("\n"))
}