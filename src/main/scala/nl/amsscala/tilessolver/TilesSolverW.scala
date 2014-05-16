package nl.amsscala
package tilessolver

import java.awt.Dimension
import scala.swing.{ BoxPanel, Button, Component, event, GridPanel, Label }
import scala.swing.{ Orientation, ScrollPane, SimpleSwingApplication, TextArea }
import javax.swing.ImageIcon
import java.awt.Cursor

object TilesSolverW extends ViewTilesSolver {
  val blancImg = new ImageIcon(resourceFromClassloader("resources/TileXX.png"))

  private var givenTiles_ : TilesToUse = Nil

  def givenTiles = givenTiles_

  def changeInput(tiles: TilesToUse) {
    givenTiles_ = tiles
    given.text = s"${givenTiles.mkString("\n")}"
    mainPanel.cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)
    lblStatusField.text = s"Computing for ${givenTiles_.size} tiles ..."
    procesSituation(givenTiles)
    mainPanel.cursor = Cursor.getDefaultCursor()
  }

  val given = new TextArea("Given tiles", 14, 10) { editable = false }
  val middle = new TextArea("Combination", 14, 10) { editable = false }
  val output = new TextArea("Tessellation", 14, 16) { editable = false }
  val mainPanel = new BoxPanel(Orientation.Horizontal) {
    contents += tileBoard
    contents += new ScrollPane(given) { horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never }
    contents += new ScrollPane(middle) { horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never }
    contents += new ScrollPane(output) { horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never }
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
          } // event.MouseClicked
        }
      } // for yield & def buttonsSeq 

      contents ++= buttonsSeq
    } // def tileBoard

  def displaySelected(solutions: Int,
                      longestLen: Int,
                      nAllLongestSolutions: Int,
                      tiles2d: Map[(Int, Int), Tile]) {
    /** Place tiles in a grid */
    def placeTilesInGrid(toDraw: Map[(Int, Int), Tile]): GridPanel = {
      // Compute the extremes, Most Top Left and the Most Bottom Right
      val (mostTopLeft, mostBottomRight) = TilesSolver.computeExtremes(toDraw)

      new GridPanel(1 + mostBottomRight._2 - mostTopLeft._2, 1 + mostBottomRight._1 - mostTopLeft._1) {
        contents ++= (for {
          y <- mostTopLeft._2 to mostBottomRight._2
          x <- mostTopLeft._1 to mostBottomRight._1
        } yield new Label {
          val tile = toDraw.get((x, y))
          icon = if (tile.isDefined) getImage(tile.get) else blancImg
        })
      }
    } // def placeTilesInGrid

    val overlaps = longestLen - tiles2d.size
    lblStatusField.text =
      s"Found ${solutions} solution(s), ${nAllLongestSolutions} are the longest, all ${longestLen} long, overlaps: $overlaps."

    mainPanel.contents(4).visible = false // This does the trick of redraw the outputGrid
    mainPanel.contents(4) =
      if (longestLen != 0) {
        output.text = (tiles2d.mkString("\n"))
        placeTilesInGrid(tiles2d)
      } else { output.text = ""; new BoxPanel(Orientation.Vertical) /*Clear text box*/ }
  }

  def procesSituation(givenTiles: TilesToUse) {

    ////////////////////////////// procesSituation /////////////////////////////
    val solution = TilesSolver.findChains(givenTiles)
    val longestLen = solution.foldLeft(0)(_ max _.size)
    // Get the longest paths
    val allLongestSolutions = solution.filter(_.length >= longestLen)

    ViewMenu.buildSolutionsMenu(solution.size, longestLen, allLongestSolutions.size, allLongestSolutions)

    //    val oneOfTheVirtualTileLayouts =
    //     if (longestLen == 0) Nil else TilesSolver.computeTilesIn2D(allLongestSolutions.head)
    //
    //    displaySelected(solution.size, longestLen, allLongestSolutions.size, oneOfTheVirtualTileLayouts.toMap)

  } // def procesSituation
} // Object ViewTilesSolver
