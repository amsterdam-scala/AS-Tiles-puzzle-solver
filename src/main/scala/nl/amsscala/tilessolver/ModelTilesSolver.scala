package nl.amsscala
package tilessolver

trait ModelTilesSolver {
  def changeInput(tiles: TilesToUse) = ModelTilesSolver.changeInput(tiles)
  def givenTiles: TilesToUse = ModelTilesSolver.givenTiles
}

object ModelTilesSolver {

  def givenTiles = ModelTilesSolver.givenTiles_

  private var givenTiles_ : TilesToUse = Nil

  def changeInput(tiles: TilesToUse) {
    givenTiles_ = tiles
    //    updateGUI
  }

}