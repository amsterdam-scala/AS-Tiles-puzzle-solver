package nl.amsscala
package tilessolver

import java.awt.{ Cursor, Dimension }
import java.awt.{ Graphics, Graphics2D }
import java.awt.print.{ PageFormat, Printable }
import java.awt.print.Printable.{ NO_SUCH_PAGE, PAGE_EXISTS }
import scala.swing.{ BoxPanel, Button, event, GridPanel, Label }
import scala.swing.{ Orientation, ScrollPane, SimpleSwingApplication, TextArea }

object TilesSolverW extends ViewTilesSolver {
  private val blancImg = new javax.swing.ImageIcon(resourceFromClassloader("resources/TileXX.png"))

  private var givenTiles_ : TilesToUse = Nil
  def givenTiles = givenTiles_
  var rawSolutions: Set[Chain] = _

  def changeInput(tiles: TilesToUse) {
    givenTiles_ = tiles
    //    given.text = s"${givenTiles.mkString("\n")}"
    mainPanel.cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)
    lblStatusField.text = s"Computing for ${givenTiles_.size} tiles ..."
    ////////////////////////////// procesSituation /////////////////////////////
    rawSolutions = TilesSolver.findChains(givenTiles)
    updateMiddle()
  }

  def updateMiddle() {

    given.text = s"${givenTiles.mkString("\n")}"

    val solutions = TilesSolver.filterRealSolutions(rawSolutions, !ViewMenu.chkNoOverlap.selected)

    val longestLen = solutions.foldLeft(0)(_ max _.size)
    // Get the longest paths
    val allLongestSolutions = solutions.filter(_.length >= longestLen)

    //middle.text = allLongestSolutions.headOption.getOrElse(Nil).mkString("\n")

    ViewMenu.buildSolutionsMenu(solutions.size, longestLen, allLongestSolutions.size, allLongestSolutions)

    mainPanel.cursor = Cursor.getDefaultCursor()
  }

  private val given = new TextArea("Given tiles", 14, 10) { editable = false }
  private val middle = new TextArea("Combination", 14, 10) { editable = false }
  private val output = new TextArea("Tessellation", 14, 16) { editable = false }

  val mainPanel = new BoxPanel(Orientation.Horizontal) with Printable {
    contents += tileBoard
    contents += new ScrollPane(given) { horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never }
    contents += new ScrollPane(middle) { horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never }
    contents += new ScrollPane(output) { horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never }
    contents += new BoxPanel(Orientation.Vertical)

    def print(g: Graphics, pf: PageFormat, page: Int) = {
      if (page <= 0) { /* We have only one page, and 'page' is zero-based */

        /* User (0,0) is typically outside the imageable area, so we must
         * translate by the X and Y values in the PageFormat to avoid clipping
         */
        val g2d = g.asInstanceOf[Graphics2D]
        g2d.translate(pf.getImageableX(), pf.getImageableY());

        /* Now print the window and its visible contents */
        this.peer.printAll(g);

        /* tell the caller that this page is part of the printed document */

        PAGE_EXISTS
      } else NO_SUCH_PAGE
    }
  } // val mainPanel

  private def tileBoard =
    new GridPanel(Directions.values.size, Directions.values.size) {

      import Directions.Directi
      private val dim = new Dimension(42, 42)

      def buttonsSeq = for {
        x <- Directions.values.view
        y <- Directions.values.view
      } yield new Button {
        def butFingerprint(x: Directi, y: Directi) = {
          val ret = if (x == y) (None, blancImg)
          else (Option(Tile(x, y)), getTileImage(Tile(x, y)))
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

  /** Place tiles in a grid */
  def displaySelected(solutions: Int,
                      longestLen: Int,
                      nAllLongestSolutions: Int,
                      selTiles: Chain) {

    def placeTilesInGrid(toDraw: Map[(Int, Int), Tile]): GridPanel = {
      // Compute the extremes, Most Top Left and the Most Bottom Right
      val (mostTopLeft, mostBottomRight) = TilesSolver.computeExtremes(toDraw)

      new GridPanel(1 + mostBottomRight._2 - mostTopLeft._2, 1 + mostBottomRight._1 - mostTopLeft._1) {
        contents ++= (for {
          y <- mostTopLeft._2 to mostBottomRight._2
          x <- mostTopLeft._1 to mostBottomRight._1
        } yield new Label {
          val tile = toDraw.get((x, y))
          icon = if (tile.isDefined) { tooltip = tile.get.toString(); getTileImage(tile.get) }
          else blancImg
        })
      }
    } // def placeTilesInGrid

    middle.text = selTiles.mkString("\n")
    val tiles2D = TilesSolver.tileSetter(selTiles)
    val overlaps = longestLen - tiles2D.size

    lblStatusField.text =
      s"Found ${solutions} solution(s), ${nAllLongestSolutions} are the longest, all ${longestLen} long, overlaps: $overlaps."

    mainPanel.contents(4).visible = false // This does the trick of redraw the outputGrid
    mainPanel.contents(4) =
      if (longestLen != 0) {
        output.text = tiles2D.mkString("\n")
        placeTilesInGrid(tiles2D)
      } else { output.text = ""; new BoxPanel(Orientation.Vertical) /*Clear text box*/ }
  }
} // Object ViewTilesSolver
