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
 *  Theoretically should the outcome be
 *
 *  Set[Set[Path]] a.k.a. Set[Set[List[Tile]]]
 *  because out the leftover tiles eventually
 *  more path(s) could be found.
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

    def walk(trail: TilesToUse, //Comparative objects A
             candidates: TilesToUse, //comparative objects B
             onHand: TilesToUse, //Actual unused tiles
             explored: Path, //Actual promising combinations in progress
             maintainedPaths: Set[Path] //List of paths already discovered
             ): Set[Path] = {
      if (trail.isEmpty) maintainedPaths + explored // distinct of a set is necessary, don't know why
      else if (candidates.isEmpty) { // Try a new walk 
        walk(trail.tail,
          tilesNotEndingInTheMiddle,
          onHand = tilesNotEndingInTheMiddle,
          Nil,
          if (explored.isEmpty || (explored.head.start != C)) maintainedPaths else maintainedPaths + explored)
      } else // Do a matching with each other tile
        walk(trail, candidates.tail, onHand, explored, maintainedPaths ++
          (if (trail.head.start isJoinable candidates.head.end) { // if jointable
            walk( // Start of actual walk parameter list
              List(candidates.head), // explore further with new found tile
              // Invoke a complete found ending sequence by empty list
              if (candidates.head.start == C) Nil else onHand diff List(candidates.head),
              onHand diff List(candidates.head), // explore further without used tile
              explored = // If ending tile save 2 tiles, including the ending one
                (if (candidates.head.start == C) List(candidates.head, trail.head) else List(trail.head))
                  ++ explored,
              maintainedPaths) // End of actual walk parameter list
          } else Nil))
    } // def walk(

    walk(trail = tiles.filter(_.end == C).distinct, // Start with ending tiles, one of each
      candidates = tilesNotEndingInTheMiddle.distinct, // Other tiles to compare with, one of each
      onHand = tilesNotEndingInTheMiddle, // Nearly all tiles to start over
      explored = Nil, // intermediate result
      maintainedPaths = Set.empty)
  } // def findPaths(

  val result = findPaths(List( // Accumulated results
    Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(W, S), Tile(C, E), Tile(S, W),
    Tile(N, E), Tile(N, S), Tile(W, C)))

  println(s"Number of unique paths: ${result.size - 1}, solution(s):")
  println(result.filter(_.length == result.maxBy(_.length).length).mkString("\n"))
}