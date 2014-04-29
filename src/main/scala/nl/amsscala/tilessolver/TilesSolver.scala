package nl.amsscala
package tilessolver

import scala.annotation.tailrec

/** Tile solver program
 *
 *  Signature is findPaths(tiles: TilesToUse): Set[Path]
 *  where TilesToUse is an unordered List[Tile]
 *  and   Path is an ordered List[Tile]
 *  so Set[Path] = Set[List[Tile]]
 *
 *  Theoretically the outcome should be:
 *
 *  Set[List[Path]] a.k.a. Set[List[List[Tile]]]
 *  because out the leftover tiles eventually
 *  more coexisting path(s) could be found.
 *  e.g. Set(List(List(Tile(C,E),Tile(W,C)),
 *                List(Tile(C,E),Tile(W,C))))
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

      /** Test if the sides of titles pair could be adjacent
       *  The function return true if the ending side meets a legal start side.
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
   *  @throws
   */
  case class Tile(val start: Direction, val end: Direction) {
    require(start != end, s"Not a proper tile definition, given $start, $end are the same.")
  }

  type Path = List[Tile]
  type TilesToUse = Path

  val l = List(Tile(C, E), Tile(W, C))

  /** Returns a set of possible paths starting
   *  and ending with a start and ending tile.
   */
  def findPaths(tiles: TilesToUse): Set[Path] = {
    /** Available tiles to combine with. */
    val tilesNotEndingInTheMiddle = tiles.filter(_.end != C)

    case class AssetHandling(val candidates: TilesToUse = tilesNotEndingInTheMiddle, //comparative objects B
                             onHand: TilesToUse = tilesNotEndingInTheMiddle, //Actual unused tiles
                             outHand: Path = Nil //Actual promising combinations in progress
                             ) {

      def completeFoundEndingSequence(previousTile: Tile) =
        {
          // Test if we found a path with a center ending tile

          val rest = if (candidates.head.start == C) Nil else onHand diff List(candidates.head)

          val assetToTransfer =
            if (candidates.head.start == C) List(candidates.head, previousTile) else List(previousTile)

          // Invoke a complete found ending sequence by empty list
          AssetHandling(if (candidates.head.start == C) Nil else onHand diff List(candidates.head),
            onHand diff List(candidates.head), // explore further without used tile
            // If ending tile save 2 tiles, including the ending one
            assetToTransfer ++ outHand)
        }

    }

    def walk(trail: TilesToUse, //Comparative objects A
             asset: AssetHandling,
             maintainedPaths: Set[Path] /*List of paths already discovered*/ ): Set[Path] = {
      if (trail.isEmpty) maintainedPaths + asset.outHand // distinct of a set is necessary, don't know why
      else if (asset.candidates.isEmpty) { // Try a new walk 
        walk(trail.tail, AssetHandling(),
          if (asset.outHand.isEmpty || (asset.outHand.head.start != C)) maintainedPaths
          else maintainedPaths + asset.outHand)
      } else // Do a matching with each other tile
        walk(trail, AssetHandling(asset.candidates.tail, asset.onHand, asset.outHand),

          maintainedPaths ++
            (if (trail.head.start isJoinable asset.candidates.head.end) {
              walk( // Start of actual walk parameter list
                List(asset.candidates.head), // explore further with new found tile

                asset.completeFoundEndingSequence(trail.head),

                maintainedPaths) // End of actual walk parameter list
            } else Nil))
    } // def walk(

    walk(trail = tiles.filter(_.end == C).distinct, // Start with ending tiles, one of each
      AssetHandling(candidates = tilesNotEndingInTheMiddle.distinct),
      maintainedPaths = Set.empty)
  } // def findPaths(

  val result = findPaths(List( // The example on the photo
    Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(W, S),
    Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C)))

  println(s"Number of unique paths: ${result.size - 1}, solution(s):")
  println(result.filter(_.length == result.maxBy(_.length).length).mkString("\n"))
}