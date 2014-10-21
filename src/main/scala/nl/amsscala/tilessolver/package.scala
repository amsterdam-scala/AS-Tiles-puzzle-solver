package nl.amsscala

/** Tile solver program
  *
  * Find the longest chain of connected tiles.
  *
  * Signature is `findChains(tiles: Chain): Set[Chain]`
  * where `Chain` is a sequential `Seq[Tile]`
  * so `Set[Chain] = Set[Seq[Tile]]`
  *
  *
  * The first element of a `Chain` holds the start tile and the last element the ending tile.
  *
  * If we take the left-overs in account for multiple Chains per solution, the outcome should be:
  * `Set[Seq[Chain]]` a.k.a. `Set[Seq[Seq[Tile]]]` because with the leftover tiles more coexisting chain(s)
  * could be found, eventually duplicates. E.g. `Set(List(Seq(Tile(C,E),Tile(W,C)), Seq(Tile(C,E),Tile(W,C))))`
  *
  * But this has to be left for a future exercise.
  *
  * @author A'dam Scala Tiles-puzzle-solver team
  */

package object tilessolver {
  /** A sequential List of a valid combination of tiles in fixed positions expresses a candidate solution. */
  type Chain = Seq[Tile]

  type Coord = (Int, Int) // Coordinate
  /** A tile with a computed position and serial number */
  type LayedTile = (Coord, (Tile, Int /*Serial number */ ))

  /** Enumeration of the connection side of a tile */
  object Directions extends Enumeration {
    /** Side names of Tiles as an enumeration */
    final val C, N, E, S, W = Directi() // Center, North, East, South ...

    case class Directi() extends Val {
      /** Returns allowed tile side chain-joint. */
      private def allowedAdjacent =
        this match {
          case N => S
          case E => W
          case S => N
          case W => E
        }

      /** Test if the sides of titles pair could be adjacent.
        * The function returns true if the ending side meets a legal terminating side.
        */
      def isJoinable = (this != C) && (_: Directi) == allowedAdjacent

      /** Given this Direction and the coordinate compute the coordinate of the next tile. */
      private[tilessolver] def step(ori: Coord): Coord = {
        this match {
          case N => (ori._1, ori._2 - 1) // Northward
          case E => (ori._1 + 1, ori._2) // Eastward
          case S => (ori._1, ori._2 + 1) // Southward
          case W => (ori._1 - 1, ori._2) // Westward
          case _ => (ori._1, ori._2) // stationary
        }
      }
    } // case class Directi()

  } // object Directions

  /** Descriptor for a tile, direction indicated with a arrow
    * @param	start The from or incoming of tile (tail of arrow)
    * @param	end	The to or outgoing side of tile (arrowhead)
    * @throws	java.lang.IllegalArgumentException If start and end are the same.
    */
  case class Tile(start: Directions.Directi, end: Directions.Directi) {
    require(start != end, s"Not a proper tile definition, given $start, $end are the same.")

    def isEndingTile = this.end == Directions.C

    def isValidAdjacent(that: Tile) = that.end.isJoinable(start)

    def whereIsNextLayed(ori: Coord): Coord = end.step(ori)
  }

} // package tilesolver