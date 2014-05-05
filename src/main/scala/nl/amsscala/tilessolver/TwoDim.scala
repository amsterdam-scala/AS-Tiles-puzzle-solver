package nl.amsscala
package tilessolver

object TwoDim extends App {
  /** Convert List[Tile] to Map[coordinate,Tile]
   */

  import Directions._

  def step(stap: Directi, ori: (Int, Int)) = stap match {
    case N => (ori._1, ori._2 + 1)
    case E => (ori._1 + 1, ori._2)
    case S => (ori._1, ori._2 - 1)
    case W => (ori._1 - 1, ori._2)
    case _ => (ori._1, ori._2)
  }

  def listWithSum(numbers: List[Int]) = numbers.foldLeft((List[Int](), 0)) {
    (resultingTuple, currentCoord) =>
      (currentCoord :: resultingTuple._1, currentCoord + resultingTuple._2)
  }

  def toDim(chain: Chain) =
    chain.foldLeft(List[((Int, Int), Tile)](), (0, 0)) {
      (resultingTuple, currentTile) =>
        (((resultingTuple._2, currentTile)) :: resultingTuple._1, step(currentTile.end, resultingTuple._2))
    }._1.toMap

  println(toDim(List(Tile(C, E), Tile(W, S), Tile(N, S), Tile(N, E), Tile(W, E), Tile(W, C))))

}