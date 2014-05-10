package nl.amsscala.tilessolver

import java.awt.{ Color, Dimension, Toolkit }
import scala.swing.{ BorderPanel, BoxPanel, Button, Component }
import scala.swing.{ FlowPanel, GridPanel, Label, MainFrame, Orientation }
import scala.swing.{ ScrollPane, SimpleSwingApplication, TextArea, event }
import javax.swing.ImageIcon

object TilesSolverW extends SimpleSwingApplication {

  def applicationTitle = "Tiles Puzzle Solver"
  def applicationShort = "Tiles solver"

  private val dim = new Dimension(42, 42)
  val blancImg = new ImageIcon(resourceFromClassloader("resources/TileXX.png"))

  def getImage(tile: Tile) = {
    new ImageIcon(resourceFromClassloader(s"resources/Tile${tile.start}${tile.end}.png"))
  }

  import Directions._
  private var givenTiles: TilesToUse = Nil

  def changeInput(tiles: TilesToUse) {
    givenTiles = tiles
    //    updateGUI
  }

  object lblStatusField extends Label {
    text = ViewMenu.t("statusMessageLabel.text")
    horizontalAlignment = scala.swing.Alignment.Left
  }

  def ui(toolbar: Option[Component] = None) = new BorderPanel() {

    val given = new TextArea(5, 20) { editable = false }
    val output = new TextArea(5, 20) { editable = false }

    def procesSituation(givenTiles: TilesToUse) {
      val solution = TilesSolver.findChains(givenTiles)
      val longestLen = solution.foldLeft(0)(_ max _.size)
      val oneOfTheSolutions =
        Tessellation.toDim(solution.filter(_.length >= longestLen).headOption.getOrElse(Nil))

      output.text_=(oneOfTheSolutions.mkString("\n"))
      if (!oneOfTheSolutions.isEmpty) {
        mainPanel.contents(3).visible = false // This does the trick of redraw the outputGrid
        mainPanel.contents(3) = outputGrid(oneOfTheSolutions)
      }
    }

    def outputGrid(toDraw: Map[(Int, Int), Tile]): GridPanel = {
      val extremes = Tessellation.computeExtremes(toDraw) // Compute the extremes, Most Top Left and the Most Bottom Right
      val (min, max) = (extremes._1, extremes._2)

      new GridPanel(1 + max._2 - min._2, 1 + max._1 - min._1) {
        contents ++= {
          for (
            y <- min._2 to max._2;
            x <- min._1 to max._1
          ) yield new Label {
            val tile = toDraw.get((x, y))
            icon = if (tile.isDefined) getImage(tile.get) else blancImg
          }
        }
      }
    }

    def buttonsSeq = for {
      x <- Directions.values.view
      y <- Directions.values.view
    } yield new Button {
      def butFingerprint(x: Directi, y: Directi) = {
        val ret = if (x == y) (None, blancImg)
        else (Option(Tile(x, y)), getImage(Tile(x, y)))
        tooltip = ret._1.getOrElse(None).toString()
        icon = ret._2
        ret._1 // Return tile
      } // def butFingerprint

      contentAreaFilled = true
      background = Color.GRAY

      minimumSize = dim
      preferredSize = minimumSize
      maximumSize = minimumSize

      val tile = butFingerprint(x.asInstanceOf[Directi], y.asInstanceOf[Directi])
      listenTo(mouse.clicks)
      reactions += {
        case click: event.MouseClicked if tile.isDefined => {
          givenTiles ++= List(tile.get)
          given.text = s"$givenTiles\n"
          procesSituation(givenTiles)
        } // event.MouseClicked
      }
    } // for yield & def buttonsSeq 

    def tileBoard: Component =
      new GridPanel(Directions.values.size, Directions.values.size) { contents ++= buttonsSeq }

    val mainPanel = new BoxPanel(Orientation.Vertical) {
      contents += tileBoard
      contents += new ScrollPane(given)
      contents += new ScrollPane(output)
      contents += new BoxPanel(Orientation.Vertical)
    }

    private val statusBar = new FlowPanel(FlowPanel.Alignment.Leading)(lblStatusField)

    // Start of UI view
    if (!toolbar.isEmpty) add(toolbar.get, BorderPanel.Position.North)
    layout(mainPanel) = BorderPanel.Position.Center
    layout(statusBar) = BorderPanel.Position.South
  } // def ui

  def top = new MainFrame {
    title = applicationTitle
    menuBar = ViewMenu.menuBar
    contents = ui()
    centerOnScreen
  }
}