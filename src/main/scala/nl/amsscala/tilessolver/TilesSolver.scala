package nl.amsscala
package tilessolver

import scala.annotation.tailrec

object TilesSolver extends App {

  /** Enumeration of the connection side of a tile*/
  object Direction extends Enumeration {
    case class Direction extends Val {
      /** Possible  tile chain-joint*/
      def legalAdjacent =
        this match {
          case N => S
          case E => W
          case S => N
          case W => E
        }

      /** Test if the sides of titles pair could be adjacent*/
      def isJoinable(adjacent: Direction) = (this != C) && adjacent == legalAdjacent
    }
    /** Side names of Tiles */
    val C, N, E, S, W = Direction() // Center, North, East, South ...
  }

  import Direction._

  /** Descriptor for a tile
   *  @param	start The from or input side of tile
   *  @param	end	The to or output side of tile indicated as capped by a triangle
   */
  case class Tile(val start: Direction, val end: Direction)

  type Path = List[Tile]

  val l = List(Tile(C, E), Tile(W, C))

  val l2 = List(
    Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(W, S),
    Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C))

  val l3 = List(
    Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(E, S),
    Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C))

  /** @param	trail	Comparative objects A
   *  @param	combinator	comparative objects B
   *  @param	source	Available tiles to combine with
   *  @param	onHand	Actual tiles left over
   *  @param	explored	Actual combination
   *  @param	accu	List of paths discovered
   */
  def walk(
    trail: List[Tile],
    combinator: List[Tile],
    source: List[Tile],
    onHand: List[Tile],
    explored: Path,
    accu: List[Path]): List[Path] = {
    if (trail.isEmpty) (explored +: accu)//.distinct // distinct is necessary, don't know why
    else if (combinator.isEmpty) { // Try a new walk 
      walk(trail.tail,
        source,
        source,
        onHand = source,
        Nil,
        if (explored.isEmpty) accu else explored +: accu)
    } else
      walk(trail, combinator.tail, source, onHand, explored,
        (if (trail.head.start isJoinable combinator.head.end) {
          walk(
            trail = List(combinator.head), // explore further with new found tile
            // Invoke a found sequence by empty list
            combinator = if (combinator.head.start == C) Nil else onHand diff List(combinator.head),
            source = source,
            onHand = onHand diff Vector(combinator.head), // explore further without used tile
            explored = (if (combinator.head.start == C) List(combinator.head, trail.head)
            else List(trail.head)) ++ explored,
            accu = accu)
        } else List.empty) ++ accu).distinct
  }

  def solve(tiles: List[Tile]) = {
    val tilesNotEndingInTheMiddle = tiles.filter(_.end != C)
    walk(trail = tiles.filter(_.end == C).distinct, // Start with ending tiles, one of each
      combinator = tilesNotEndingInTheMiddle.distinct, // Other tile to compare with, one of each
      source = tilesNotEndingInTheMiddle, // available tiles to compose with
      onHand = tilesNotEndingInTheMiddle, // Nearly all tiles to start over
      explored = Nil, // intermediate result
      accu = Nil)
  }

  println(solve(l2).distinct.mkString("\n")) // Accumulated results

}