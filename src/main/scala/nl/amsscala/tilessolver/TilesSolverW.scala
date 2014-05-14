package nl.amsscala
package tilessolver

import java.awt.Dimension
import scala.swing.{ BoxPanel, Button, Component, event, GridPanel, Label }
import scala.swing.{ Orientation, ScrollPane, SimpleSwingApplication, TextArea }
import javax.swing.ImageIcon

object TilesSolverW extends ViewTilesSolver {
  val blancImg = new ImageIcon(resourceFromClassloader("resources/TileXX.png"))

  private var givenTiles_ : TilesToUse = Nil

  def givenTiles = givenTiles_

  def changeInput(tiles: TilesToUse) {
    givenTiles_ = tiles
    procesSituation(givenTiles)
  }

  val given = new TextArea(5, 20) { editable = false }
  val output = new TextArea(5, 20) { editable = false }
  val mainPanel = new BoxPanel(Orientation.Vertical) {
    contents += tileBoard
    contents += new ScrollPane(given)
    contents += new ScrollPane(output)
    contents += new BoxPanel(Orientation.Vertical)
  }

  def tileBoard: Component =
    new GridPanel(Directions.values.size, Directions.values.size) {

      import Directions._
      private val dim = new Dimension(42, 42)

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

        val tile = butFingerprint(x.asInstanceOf[Directi], y.asInstanceOf[Directi])
        minimumSize = dim
        preferredSize = minimumSize
        maximumSize = minimumSize

        listenTo(mouse.clicks)
        reactions += {
          case click: event.MouseClicked if tile.isDefined => {
            changeInput(givenTiles ++ List(tile.get))
            given.text = s"$givenTiles\n"

          } // event.MouseClicked
        }
      } // for yield & def buttonsSeq 

      contents ++= buttonsSeq
    } // def tileBoard

  def procesSituation(givenTiles: TilesToUse) {
    val solution = TilesSolver.findChains(givenTiles)
    val longestLen = solution.foldLeft(0)(_ max _.size)

    def outputGrid(toDraw: Map[(Int, Int), Tile]): GridPanel = {
      val extremes = TilesSolver.computeExtremes(toDraw) // Compute the extremes, Most Top Left and the Most Bottom Right
      val (min, max) = (extremes._1, extremes._2)

      new GridPanel(1 + max._2 - min._2, 1 + max._1 - min._1) {
        contents ++= (for {
          y <- min._2 to max._2
          x <- min._1 to max._1
        } yield new Label {
          val tile = toDraw.get((x, y))
          icon = if (tile.isDefined) getImage(tile.get) else blancImg
        })
      }
    } // def outputGrid

    mainPanel.contents(3).visible = false // This does the trick of redraw the outputGrid
    mainPanel.contents(3) = if (longestLen != 0) {

      val oneOfTheSolutions =
        TilesSolver.placeTiles(solution.filter(_.length >= longestLen).head)

      output.text = (oneOfTheSolutions.mkString("\n"))

      outputGrid(oneOfTheSolutions.toMap // TODO "Escher’s Effect by toMap"
      )
    } else {
      output.text = ""
      new BoxPanel(Orientation.Vertical)
    }
  }

} // Object ViewTilesSolver
