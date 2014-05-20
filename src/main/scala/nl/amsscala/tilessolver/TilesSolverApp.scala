package nl.amsscala
package tilessolver

import java.awt.{ Cursor, Dimension }
import java.awt.{ Graphics, Graphics2D }
import java.awt.print.{ PageFormat, Printable }
import java.awt.print.Printable.{ NO_SUCH_PAGE, PAGE_EXISTS }
import scala.swing.{ BoxPanel, Button, event, GridPanel, Label }
import scala.swing.{ Orientation, Panel, ScrollPane, SimpleSwingApplication, TextArea }
import scala.swing.Swing.VGlue
import java.awt.print.PrinterException
import java.awt.print.PrinterJob

object TilesSolverApp extends ViewTilesSolver {
  System.setProperty("apple.laf.useScreenMenuBar", "true")

  private val blancImg = new javax.swing.ImageIcon(resourceFromClassloader("resources/TileXX.png"))
  private val dim = new Dimension(42, 42)
  private var givenTiles_ : TilesToUse = Nil
  def givenTiles = givenTiles_
  var rawSolutions: Set[Chain] = Set()

  def changeInput(tiles: TilesToUse) {
    givenTiles_ = tiles
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
    val allLongestSolutions = solutions.filter(_.size >= longestLen)

    ViewMenu.buildSolutionsMenu(solutions.size, longestLen, allLongestSolutions.size, allLongestSolutions)

    mainPanel.cursor = Cursor.getDefaultCursor()
  }

  private val (given, middle, output) =
    (new TextArea("Given tiles", 14, 10) { editable = false },
      new TextArea("Combination", 14, 10) { editable = false },
      new TextArea("Tessellation", 14, 16) { editable = false })

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
        //        val g2d = g.asInstanceOf[Graphics2D]
        //        g2d.translate(pf.getImageableX(), pf.getImageableY())

        /* Now print the window and its visible contents */
        this.peer.printAll(g)

        /* tell the caller that this page is part of the printed document */

        PAGE_EXISTS
      } else NO_SUCH_PAGE
    }

    def doPrint {
      val job = PrinterJob.getPrinterJob()
      job.setPrintable(this)
      if (job.printDialog()) job.print()
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
        def butFingerprint(x: Directi, y: Directi): Option[Tile] = {
          val ret = if (x == y) (None, blancImg) else ((Option(Tile(x, y)), getTileImage(Tile(x, y))))
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

    def placeTilesInGrid(toDraw: Map[(Int, Int), (Tile, Int)], dblure: Set[(Int, Int)]): Panel = {
      // Compute the extremes, Most Top Left and the Most Bottom Right
      val (mostTopLeft, mostBottomRight) = TilesSolver.calculateExtremes(toDraw)
      new BoxPanel(Orientation.Vertical) {
        contents +=
          new GridPanel(1 + mostBottomRight._2 - mostTopLeft._2, 1 + mostBottomRight._1 - mostTopLeft._1) {
            contents ++= (for {
              y <- mostTopLeft._2 to mostBottomRight._2
              x <- mostTopLeft._1 to mostBottomRight._1
            } yield new Button {
              focusable=false
              this.
              minimumSize = dim
              preferredSize = dim
              val tile = toDraw.get((x, y))

              if (dblure.contains((x, y))) background = java.awt.Color.RED
              icon = if (tile.isDefined) { tooltip = tile.get.toString(); getTileImage(tile.get._1) }
              else blancImg
            })
          }
        contents ++= List(VGlue)
      }
    } // def placeTilesInGrid

    middle.text = selTiles.mkString("\n")

    val tiles2Draw = TilesSolver.virtualLayoutTiles(selTiles)
    val tiles2D = tiles2Draw.toMap

    val doublures = TilesSolver.virtualLayoutTiles(selTiles).groupBy(_._1).filter(_._2.lengthCompare(1) > 0).keySet

    lblStatusField.text =
      s"Found ${solutions} solution(s), ${nAllLongestSolutions} are the longest, all ${longestLen} long, overlaps: ${doublures.size}."

    mainPanel.contents(4).visible = false // This does the trick of redraw the outputGrid
    mainPanel.contents(4) =
      if (longestLen != 0) {
        output.text = tiles2D.toList.sortBy { _._2._2 }.map(x => s"${x._1} ${x._2._1}").mkString("\n")
        placeTilesInGrid(tiles2D, doublures)
      } else { output.text = ""; new BoxPanel(Orientation.Vertical) /*Clear text box*/ }
  }
} // Object ViewTilesSolver