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

  type Path = Vector[Tile]

  implicit class PathWrapper(path: Path) {
    def isPrependable(tile: Tile): Boolean =
      if (path.isEmpty) tile.end == C else path.head.start isJoinable tile.end
  }

  @tailrec
  def solution(itemToTest: Int, available: Vector[Tile], path: Path): Path = {
    if (itemToTest < 0) path
    else {
      if (path.head.start isJoinable available(itemToTest).end)
        solution(available.size - 2, available diff Vector(available(itemToTest)), available(itemToTest) +: path)
      else solution(itemToTest - 1, available, path)
    }
  }

  val l = List(Tile(C, E), Tile(W, C))

  val l2 = Vector(
    Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(W, S),
    Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C))

  val l3 = Vector(
    Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(E, S),
    Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C))

  @tailrec
  def solver(startItems: Vector[Tile], index: Int, accu: List[Path]): List[Path] = {
    if (index < 0) accu
    else solver(startItems, index - 1, accu :+ solution(startItems.size - 1, startItems, Vector(startItems(index))))
  }

  def walk(trail: Vector[Tile],
           combinator: Vector[Tile],
           source: Vector[Tile],
           onHand: Vector[Tile],
           explored: Path,
           accu: Vector[Path]): Vector[Path] = {
    //println("Trail " + trail + " comb " + combinator + " expl " + explored + " accu " + accu)
    if (trail.isEmpty) explored +: accu
    else if (combinator.isEmpty) { // Try a new walk 
      walk(trail.tail,
        source.distinct,
        source,
        onHand = source,
        Vector.empty,
        if (explored.isEmpty) accu else explored +: accu)
    } else
      walk(trail, combinator.tail, source, onHand, explored, accu ++
        (if (trail.head.start isJoinable combinator.head.end) {
          walk(trail = Vector(combinator.head), // explore further with new found tile
            combinator = onHand.tail, // diff Vector(combinator.head), // and continue with remaining tiles
            source = source,
            onHand = onHand.tail,// diff Vector(combinator.head), // explore further without used tile
            explored = if (explored.isEmpty) Vector(trail.head) else trail.head +: explored,
            accu = accu)
        } else Vector.empty))
  }

  def solve(tiles: Vector[Tile]) = {
    walk(tiles.filter(_.end == C).distinct, // Start with ending tiles, one of each
      (tiles.filter(_.end != C)), // Other tile to compare with, one of each
      (tiles.filter(p =>(p.end != C) ||( p.start != C))), // available tile to compose with
      (tiles.filter(p =>(p.end != C) ||( p.start != C))), // Nearly all tiles to start over
      Vector.empty, // intermediate result
      Vector.empty)
  }

  println(solve(l2).distinct.mkString("\n")) // Accumulated results

}