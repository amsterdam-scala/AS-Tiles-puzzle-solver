package nl.amsscala
package tilessolver

import scala.annotation.tailrec

/** @author A'dam Scala Tiles-puzzle-solver team
 */

object TilesSolver extends App {

  /** Enumeration of the connection side of a tile*/
  object Direction extends Enumeration {
    case class Direction() extends Val {
      /** Possible  tile chain-joint*/
      def legalAdjacent =
        this match {
          case N => S
          case E => W
          case S => N
          case W => E
        }

      /** Test if the sides of titles pair could be adjacent
       *  The function return true if the ending side meets a legal start side.
       */
      def isJoinable(adjacent: Direction) = (this != C) && adjacent == legalAdjacent
    }
    /** Side names of Tiles */
    val C, N, E, S, W = Direction() // Center, North, East, South ...
  }

  import Direction._

  /** Descriptor for a tile direction indicated with a arrow
   *  @param	start The from or incoming of tile
   *  @param	end	The to or outcoming side of tile
   */
  case class Tile(val start: Direction, val end: Direction) {
    require(start != end, s"Not a proper tile definition, given $start, $end are the same.")
  }

  type Path = List[Tile]
  type TilesToUse = Path

  val l = List(Tile(C, E), Tile(W, C))

  /** Returns a list of possible paths starting
   *  and ending with a start and ending tile.
   */
  def findPaths(tiles: TilesToUse): Set[Path] = {
    /** Available tiles to combine with. */
    val tilesNotEndingInTheMiddle = tiles.filter(_.end != C)

    def walk(trail: TilesToUse, //Comparative objects A
             candidates: TilesToUse, //comparative objects B
             onHand: TilesToUse, //Actual unused tiles
             explored: Path, //Actual promising combinations in progress
             maintainedPath: Set[Path] //List of paths already discovered
             ): Set[Path] = {
      if (trail.isEmpty) maintainedPath + explored // distinct of a set is necessary, don't know why
      else if (candidates.isEmpty) { // Try a new walk 
        walk(trail.tail,
          tilesNotEndingInTheMiddle,
          onHand = tilesNotEndingInTheMiddle,
          Nil,
          if (explored.isEmpty || (explored.head.start != C)) maintainedPath else maintainedPath + explored)
      } else // Do a matching with each other tile
        walk(trail, candidates.tail, onHand, explored, maintainedPath ++
          (if (trail.head.start isJoinable candidates.head.end) { // if jointable
            walk(
              trail = List(candidates.head), // explore further with new found tile
              // Invoke a complete found ending sequence by empty list
              candidates = if (candidates.head.start == C) Nil else onHand diff List(candidates.head),
              onHand = onHand diff List(candidates.head), // explore further without used tile
              explored = // If ending tile save 2 tiles, including the ending one
                (if (candidates.head.start == C) List(candidates.head, trail.head)
                else List(trail.head)) ++ explored,
              maintainedPath = maintainedPath)
          } else Nil))
    } // def walk(

    walk(trail = tiles.filter(_.end == C).distinct, // Start with ending tiles, one of each
      candidates = tilesNotEndingInTheMiddle.distinct, // Other tiles to compare with, one of each
      onHand = tilesNotEndingInTheMiddle, // Nearly all tiles to start over
      explored = Nil, // intermediate result
      maintainedPath = Set.empty)
  } // def solve(

  val result = findPaths(List( // Accumulated results
    Tile(C, E), Tile(N, E), Tile(N, S), Tile(W, C), Tile(N, C), Tile(W, E), Tile(W, S)))

  println(result.mkString("\n"))
}