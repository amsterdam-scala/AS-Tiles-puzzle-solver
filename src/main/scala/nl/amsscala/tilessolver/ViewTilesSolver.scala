package nl.amsscala
package tilessolver

import scala.swing.{ BorderPanel, Component, FlowPanel, Label }
import scala.swing.{ MainFrame, SimpleSwingApplication }
import javax.swing.ImageIcon

trait ViewTilesSolver extends SimpleSwingApplication {
  System.setProperty("apple.laf.useScreenMenuBar", "true")

  val applicationTitle = "Tiles Puzzle Solver"
  val applicationShort = "Tiles solver"

  def getTileImage(tile: Tile) = {
    new ImageIcon(resourceFromClassloader(s"resources/Tile${tile.start}${tile.end}.png"))
  }

  object lblStatusField extends Label {
    text = ViewMenu.t("Compose a set of tiles by clicking on the tile pad.")
    horizontalAlignment = scala.swing.Alignment.Left
  }

  private def ui(toolbar: Option[Component] = None) = new BorderPanel() {

    def statusBar = new FlowPanel(FlowPanel.Alignment.Leading)(lblStatusField)

    ///////////////////////// Start of ui //////////////////////////////////////

    if (!toolbar.isEmpty) add(toolbar.get, BorderPanel.Position.North)
    layout(TilesSolverApp.mainPanel) = BorderPanel.Position.Center
    layout(statusBar) = BorderPanel.Position.South
  } // def ui

  def top() = new MainFrame {
    title = applicationTitle
    iconImage = toolkit.getImage(getClass.getResource("resources/px-32ams-scala.png"))
    menuBar = ViewMenu.menuBar
    contents = ui()
    centerOnScreen
  }

} // trait ViewTilesSolver