package nl.amsscala

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

package object tilessolver {
  /** A sequential List of a valid combination of tiles.*/
  type Chain = List[Tile]
  /** An unordered list of tiles.*/
  type TilesToUse = Chain

  /** Enumeration of the connection side of a tile*/
  object Directions extends Enumeration {
    case class Directi() extends Val {
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
      def isJoinable(adjacent: Directi) = (this != C) && adjacent == allowedAdjacent
    }
    //    type Directions = Directi
    /** Side names of Tiles */
    val C, N, E, S, W = Directi() // Center, North, East, South ...
  } // object Directions

  import Directions._

  /** Descriptor for a tile, direction indicated with a arrow
   *  @param	start The from or incoming of tile (tail of arrow)
   *  @param	end	The to or outgoing side of tile (arrowhead)
   *  @throws	java.lang.IllegalArgumentException If start and end are the same.
   */
  case class Tile(val start: Directi, end: Directi) {
    require(start != end, s"Not a proper tile definition, given $start, $end are the same.")
  }
}