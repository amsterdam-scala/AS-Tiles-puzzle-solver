package nl.amsscala.tilessolver

import scala.swing.BorderPanel
import scala.swing.Component
import scala.swing.FlowPanel
import scala.swing.GridPanel
import scala.swing.Label
import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication
import javax.swing.ImageIcon

trait ViewTilesSolver extends SimpleSwingApplication {

  def applicationTitle = "Tiles Puzzle Solver"
  def applicationShort = "Tiles solver"

  def getImage(tile: Tile) = {
    new ImageIcon(resourceFromClassloader(s"resources/Tile${tile.start}${tile.end}.png"))
  }

  object lblStatusField extends Label {
    text = ViewMenu.t("statusMessageLabel.text")
    horizontalAlignment = scala.swing.Alignment.Left
  }

  def ui(toolbar: Option[Component] = None) = new BorderPanel() {

    private val statusBar = new FlowPanel(FlowPanel.Alignment.Leading)(lblStatusField)

    ///////////////////////// Start of ui //////////////////////////////////////

    if (!toolbar.isEmpty) add(toolbar.get, BorderPanel.Position.North)
    layout(TilesSolverW.mainPanel) = BorderPanel.Position.Center
    layout(statusBar) = BorderPanel.Position.South
  } // def ui

  def top = new MainFrame {
    title = applicationTitle
    iconImage = toolkit.getImage(getClass.getResource("resources/px-32ams-scala.png"))
    menuBar = ViewMenu.menuBar
    contents = ui()
    centerOnScreen
  }
} // trait ViewTilesSolver