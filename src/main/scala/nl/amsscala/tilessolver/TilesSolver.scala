package nl.amsscala
package nl.amsscala.tilessolver

object TilesSolver {

  object Direction extends Enumeration {
    case class Direction extends Val {
      def =!=(other: Direction) = {
        this match {
          case O => false
          case N => other == S
          case S => other == N
          case E => other == W
          case W => other == E
        }
      }
    }
    val O, N, E, S, W = Direction()
  }

  import Direction._

  case class Tile(val start: Direction, val end: Direction)

  type Path = List[Tile]

  implicit class PathWrapper(path: Path) {
    def isPrepandable(tile: Tile) = {
      if (path.isEmpty) tile.end == O
      else path.head.start =!= tile.end
    }
  }

  def solver(source: List[Tile], path: Path): List[Path] = {
    val flatmapped = source.zipWithIndex.filter(ti => path.isPrepandable(ti._1)).flatMap(ti => {
      var res = solver(source.take(ti._2) ++ source.drop(ti._2 + 1), ti._1 +: path)
      if (ti._1.start == O) res = res :+ (ti._1 +: path)
      res
    })
    flatmapped
  }
  val l = List(Tile(O, E), Tile(W, O))
  solver(l, List())

  val l2 = List(Tile(S, E), Tile(W, E), Tile(N, O), Tile(O, E), Tile(W, S), Tile(O, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, O))

  solver(l2, List()).groupBy(_.length).maxBy(_._1)._2.mkString("\n")

}