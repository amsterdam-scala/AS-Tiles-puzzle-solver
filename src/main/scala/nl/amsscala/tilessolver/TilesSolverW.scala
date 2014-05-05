package nl.amsscala.tilessolver

import java.awt.Color
import java.awt.Dimension
import scala.swing.BorderPanel
import scala.swing.Button
import scala.swing.Component
import scala.swing.GridPanel
import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication
import scala.swing.event
import javax.swing.ImageIcon
import scala.swing.TextArea
import scala.swing.ScrollPane
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.Label

object TilesSolverW extends SimpleSwingApplication {
  private val dim = new Dimension(42, 42)

  val boardSize = Directions.values.size
  val linearIndexRange = 0 until boardSize * boardSize

  var givenTiles: TilesToUse = Nil

  val given = new TextArea(5, 20) { editable = false }
  val output = new TextArea(5, 20) { editable = false }

  val blancImg = new ImageIcon(resourceFromClassloader("resources/TileXX.png"))

  def getImage(tile: Tile) = {
    new ImageIcon(resourceFromClassloader(s"resources/Tile${tile.start}${tile.end}.png"))
  }

  def buttonsSeq = {
    import Directions._

    for {
      x <- Directions.values.view
      y <- Directions.values.view
    } yield new Button {
      def butFingerprint(x: Directi, y: Directi) = {
        val ret =
          if (x == y) (None, blancImg)
          else (Option(Tile(x, y)), getImage(Tile(x, y)))
        icon = ret._2
        ret._1
      }

      contentAreaFilled = true
      background = Color.GRAY
      preferredSize = dim
      val tile = butFingerprint(x.asInstanceOf[Directi], y.asInstanceOf[Directi])
      listenTo(mouse.clicks)
      reactions += {
        case me: event.MouseClicked => {

          if (tile.isDefined) givenTiles ++= List(tile.get)
          given.text = s"$givenTiles\n"

          val result = TilesSolver.findChains(givenTiles)
          val longestLen = result.foldLeft(0)(_ max _.size)
          output.text_=(result.filter(_.length >= longestLen).mkString("\n"))

        }

      }
    }
  }

  def ui(toolbar: Option[Component] = None) = new BorderPanel() {

    private def mainPanel = {

      def keyBoard = { new GridPanel(boardSize, boardSize) { contents ++= buttonsSeq } }
      def outBoard = {
        new GridPanel(boardSize, boardSize) {

          contents += new Label {
            icon = blancImg
            preferredSize = dim
          }
        }
      }

      new BoxPanel(Orientation.Vertical) {
        contents += keyBoard
        contents += new ScrollPane(given)
        contents += new ScrollPane(output)
        contents += outBoard
      }
    } // def mainPanel 

    // Start of UI view
    if (!toolbar.isEmpty) add(toolbar.get, BorderPanel.Position.North)
    layout(mainPanel) = BorderPanel.Position.Center
    //layout(statusBar) = BorderPanel.Position.South

  }

  def top = new MainFrame {
    title = "Scala Tiles Puzzle Solver"
    contents = ui()
  }
}