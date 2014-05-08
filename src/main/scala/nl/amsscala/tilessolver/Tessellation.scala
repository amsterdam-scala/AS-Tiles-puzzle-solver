package nl.amsscala
package tilessolver

/** Convert List[Tile] to Map[coordinate,Tile]
 */
object Tessellation {
  import Directions._

  def step(stap: Directi, ori: (Int, Int)) = stap match {
    case N => (ori._1, ori._2 - 1)
    case E => (ori._1 + 1, ori._2)
    case S => (ori._1, ori._2 + 1)
    case W => (ori._1 - 1, ori._2)
    case _ => (ori._1, ori._2)
  }

  def listWithSum(numbers: List[Int]) = numbers.foldLeft((List[Int](), 0)) {
    (resultingTuple, currentCoord) =>
      (currentCoord :: resultingTuple._1, currentCoord + resultingTuple._2)
  }

  /** Place the set of tiles in a grid */
  def toDim(chain: Chain): Map[(Int, Int), Tile] =
    chain.foldLeft(List[((Int, Int), Tile)](), (0, 0)) {
      (resultingTuple, currentTile) =>
        (((resultingTuple._2, currentTile)) :: resultingTuple._1, step(currentTile.end, resultingTuple._2))
    }._1.toMap

  /** Compute the extremes, Most Top Left and the Most Bottom Right */
  def computeExtremes(toDraw: Map[(Int, Int), Tile]) =
    toDraw.keys.foldLeft((toDraw.keys.head, toDraw.keys.head)) {
      (accu, tileCoord) =>
        ((accu._1._1 min tileCoord._1, accu._1._2 min tileCoord._2),
          (accu._2._1 max tileCoord._1, accu._2._2 max tileCoord._2))
    }
}