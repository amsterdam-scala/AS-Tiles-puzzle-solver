package nl.amsscala
package tilessolver

import com.sun.xml.internal.bind.v2.TODO

/** Convert List[Tile] to Map[coordinate,Tile]
 */
object Tessellation {

  /** Compute the placement of tiles in a grid. Every tile has a direction, so
   *  the direction after each tile is known. After a tile a step is made in one
   *  of the 4 directions, this result in a increment/decrement in x or either y.
   */
  def placeTiles(chain: Chain): Map[(Int, Int), Tile] =
      chain.tail.scanLeft(((0, 0), chain.head)) {
        (resultingTuple, currentTile) => (resultingTuple._2.end.step(resultingTuple._1), currentTile)
      }.toMap // TODO "Escher’s Effect by toMap"


  /** Compute the extremes, Least Top Left and the Most Bottom Right */
  def computeExtremes(toDraw: Map[(Int, Int), Tile]) =
    toDraw.keys.tail.foldLeft((toDraw.keys.head, toDraw.keys.head)) {
      (a, tileCoord) =>
        ((a._1._1 min tileCoord._1, a._1._2 min tileCoord._2), // The LTL part of resulting tuple
          (a._2._1 max tileCoord._1, a._2._2 max tileCoord._2)) // The MBR part
    }
}