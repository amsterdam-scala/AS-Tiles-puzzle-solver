package nl.amsscala

/** Tile solver program
 *
 *  Find the longest chain of connected tiles.
 *
 *  Signature is `findChains(tiles: TilesToUse): Set[Chain]`
 *  where `TilesToUse` is an unordered `List[Tile]` and `Chain` is a sequential `List[Tile]`
 *  so `Set[Chain] = Set[List[Tile]]`
 *
 *
 *  The first element of a `Chain` holds the start tile and the last element the ending tile.
 *
 *  If we take the left-overs in account the outcome should be: `Set[List[Chain]]` a.k.a. `Set[List[List[Tile]]]`
 *
 *  because with the leftover tiles more coexisting chain(s) could be found, eventually duplicates. E.g.
 *
 *  `Set(List(List(Tile(C,E),Tile(W,C)), List(Tile(C,E),Tile(W,C))))`
 *
 *  But this has to be left for a future exercise.
 *
 *  @author A'dam Scala Tiles-puzzle-solver team
 */

package object tilessolver {
  /** A sequential List of a valid combination of tiles in fixed positions expresses a candidate solution.*/
  type Chain = List[Tile]
  /** An unordered list of tiles, commonly the items in hand to combine with.*/
  type TilesToUse = Chain
  /** A tile with a computed position and serial number */
  type LayedTile = ((Int, Int), (Tile, Int /*Serial number */ ))

  /** Enumeration of the connection side of a tile*/
  object Directions extends Enumeration {
    case class Directi() extends Val {
      /** Returns allowed tile side chain-joint.*/
      def allowedAdjacent =
        this match {
          case N => S
          case E => W
          case S => N
          case W => E
        }

      /** Test if the sides of titles pair could be adjacent.
       *  The function returns true if the ending side meets a legal terminating side.
       */
      def isJoinable = (this != C) && (_: Directi) == allowedAdjacent

      /** Given this Direction and the coordinate compute the coordinate of the next tile.*/
      def step(ori: (Int, Int)): (Int, Int) = {
        this match {
          case N => (ori._1, ori._2 - 1) // Northward
          case E => (ori._1 + 1, ori._2) // Eastward
          case S => (ori._1, ori._2 + 1) // Southward
          case W => (ori._1 - 1, ori._2) // Westward
          case _ => (ori._1, ori._2) // stationary
        }
      }
    }
    type Directions = Directi
    /** Side names of Tiles */
    final val C, N, E, S, W = Directi() // Center, North, East, South ...
  } // object Directions

  import Directions.Directi

  /** Descriptor for a tile, direction indicated with a arrow
   *  @param	start The from or incoming of tile (tail of arrow)
   *  @param	end	The to or outgoing side of tile (arrowhead)
   *  @throws	java.lang.IllegalArgumentException If start and end are the same.
   */
  case class Tile(start: Directi, end: Directi) {
    require(start != end, s"Not a proper tile definition, given $start, $end are the same.")

    def whereNext(ori: (Int, Int)): (Int, Int) = this.end.step(ori)
  }
}