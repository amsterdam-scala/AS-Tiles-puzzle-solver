package nl.amsscala
package tilessolver

import scala.annotation.tailrec

object TilesSolver {

  case class Direction {
    def =!=(other: Direction) =
      this match {
        case C => false
        case N => other == S
        case S => other == N
        case E => other == W
        case W => other == E
      }
  }
  val C, N, E, S, W = Direction()

  import Direction._

  case class Tile(val start: Direction, val end: Direction)

  type Path = List[Tile]

  implicit class PathWrapper(path: Path) {
    def isPrepandable(tile: Tile): Boolean = {
      if (path.isEmpty) tile.end == C else path.head.start =!= tile.end
    }
  }

  @tailrec
  def solver(source: List[Tile], path: Path): List[Path] = {
    val flatmapped = source.zipWithIndex.filter(ti => path.isPrepandable(ti._1)).flatMap(ti => {
      var res = solver(source.take(ti._2) ++ source.drop(ti._2 + 1), ti._1 +: path)
      if (ti._1.start == C) res = res :+ (ti._1 +: path)
      res
    })
    flatmapped
  }

  val l = List(Tile(C, E), Tile(W, C))
  solver(l, List())

  val l2 = List(Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(W, S), Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C))

  solver(l2, List()).groupBy(_.length).maxBy(_._1)._2.mkString("\n")

}