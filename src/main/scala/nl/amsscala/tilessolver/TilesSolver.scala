package nl.amsscala
package tilessolver

import scala.annotation.tailrec

/** @author A'dam Scala Tiles-puzzle-solver team
 */

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

  def solve(tiles: List[Tile]) = {
    /** Available tiles to combine with*/
    val tilesNotEndingInTheMiddle = tiles.filter(_.end != C)

    def walk(trail: List[Tile], //Comparative objects A
             combinator: List[Tile], //comparative objects B
             onHand: List[Tile], //Actual tiles left over
             explored: Path, //Actual promising combination in progress
             accu: List[Path] //List of paths discovered
             ): List[Path] = {
      if (trail.isEmpty) (explored +: accu).distinct // distinct is necessary, don't know why
      else if (combinator.isEmpty) { // Try a new walk 
        walk(trail.tail,
          tilesNotEndingInTheMiddle,
          onHand = tilesNotEndingInTheMiddle,
          Nil,
          if (explored.isEmpty || (explored.head.start != C)) accu else explored +: accu)
      } else
        walk(trail, combinator.tail, onHand, explored,
          (if (trail.head.start isJoinable combinator.head.end) {
            walk(
              trail = List(combinator.head), // explore further with new found tile
              // Invoke a complete found ending sequence by empty list
              combinator = if (combinator.head.start == C) Nil else onHand diff List(combinator.head),
              onHand = onHand diff Vector(combinator.head), // explore further without used tile
              explored =
                (if (combinator.head.start == C) List(combinator.head, trail.head)
                else List(trail.head)) ++ explored,
              accu = accu)
          } else Nil) ++ accu)
    } // def walk(

    walk(trail = tiles.filter(_.end == C).distinct, // Start with ending tiles, one of each
      combinator = tilesNotEndingInTheMiddle.distinct, // Other tiles to compare with, one of each
      onHand = tilesNotEndingInTheMiddle, // Nearly all tiles to start over
      explored = Nil, // intermediate result
      accu = Nil)
  } // def solve(

  val result = solve(l2)

  println("Diff " + (result diff result.distinct))
  println(result.mkString("\n")) // Accumulated results
}