package nl.amsscala
package tilessolver

import java.awt.print.Printable.{NO_SUCH_PAGE, PAGE_EXISTS}
import java.awt.print.{PageFormat, Printable, PrinterJob}
import java.awt.{Cursor, Graphics}
import javax.swing.ImageIcon

import scala.swing.Swing.{VGlue, pair2Dimension}
import scala.swing.{BorderPanel, BoxPanel, Button, Component, FlowPanel, GridPanel, Label, MainFrame, Orientation, Panel, ScrollPane, SimpleSwingApplication, TextArea, event}

object Model {
  type TilesToUse = Chain
  var rawSolutions: Set[Chain] = Set()

  private var givenTiles_ : TilesToUse = Nil

  def givenTiles = givenTiles_

  def changeInput(tiles: TilesToUse) {
    givenTiles_ = tiles
    TilesSolverApp.mainPanel.cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)
    TilesSolverApp.lblStatusField.text = s"Computing for ${givenTiles_.size} tiles ..."
    ////////////////////////////// procesSituation /////////////////////////////
    rawSolutions = TilesSolver.findChains(givenTiles).toSet
    Control.updateMiddle()
  }
}

trait View extends SimpleSwingApplication {
  System.setProperty("apple.laf.useScreenMenuBar", "true")

  final val applicationTitle = "Tiles Puzzle Solver"
  final val applicationShort = "Tiles solver"
  final val blancImg = getImageByPartialPath("resources/TileXX.png")
  final val dim = (42, 42)

  val (given, middle, output) =
    (new TextArea("Given tiles", 14, 10) {
      editable = false
    },
      new TextArea("Combination", 12, 10) {
        editable = false
      },
      new TextArea("Tessellation", 14, 16) {
        editable = false
      })

  def getImageByPartialPath(partPath: String) : ImageIcon=
    try {
      new ImageIcon(resourceFromClassloader(partPath))
    } catch {
      case t: Throwable =>
        println(s"$t: $partPath not found")
        throw new ExceptionInInitializerError("Resource not found")
    }


  val mainPanel = new BoxPanel(Orientation.Horizontal) with Printable {
    contents += tileBoard
    contents += new ScrollPane(given) {
      horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never
    }
    contents += new ScrollPane(middle) {
      horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never
    }
    contents += new ScrollPane(output) {
      horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never
    }
    contents += new BoxPanel(Orientation.Vertical)

    protected def print(g: Graphics, pf: PageFormat, page: Int) = {
      if (page <= 0) {
        /* We have only one page, and 'page' is zero-based */

        /* Now print the window and its visible contents */
        this.peer.printAll(g)

        /* tell the caller that this page is part of the printed document */
        PAGE_EXISTS
      } else NO_SUCH_PAGE
    }

    def doPrint() {
      val job = PrinterJob.getPrinterJob
      job.setPrintable(this)
      if (job.printDialog()) job.print()
    }
  } // val mainPanel

  private def tileBoard =
    new GridPanel(Directions.values.size, Directions.values.size) {

      import Directions.Directi

      def buttonsSeq = for {
        x <- Directions.values.view // Without view it would be in disorder
        y <- Directions.values.view
      } yield new Button {
          def butFingerprint(x: Directi, y: Directi): Option[Tile] = {
            val ret = if (x == y) (None, blancImg) else (Option(Tile(x, y)), getTileImage(Tile(x, y)))
            tooltip = ret._1.getOrElse(None).toString
            icon = ret._2
            ret._1 // Return tile
          } // def butFingerprint

          val tile = butFingerprint(x.asInstanceOf[Directi], y.asInstanceOf[Directi])
          minimumSize = dim
          preferredSize = minimumSize
          maximumSize = minimumSize

          listenTo(mouse.clicks)
          reactions += {
            case click: event.MouseClicked if tile.isDefined =>
              Model.changeInput(Model.givenTiles ++ List(tile.get))
          }
        } // for yield & def buttonsSeq

      contents ++= buttonsSeq
    } // def tileBoard

  def getTileImage(tile: Tile) =
    getImageByPartialPath(s"resources/Tile${tile.start}${tile.end}.png")

  object lblStatusField extends Label {
    text = ViewMenu.t("Compose a set of tiles by clicking on the tile pad.")
    horizontalAlignment = scala.swing.Alignment.Left
  }

  private def ui(toolbar: Option[Component] = None) = new BorderPanel() {

    def statusBar = new FlowPanel(FlowPanel.Alignment.Leading)(lblStatusField)

    ///////////////////////// Start of ui //////////////////////////////////////

    if (toolbar.nonEmpty) add(toolbar.get, BorderPanel.Position.North)
    layout(mainPanel) = BorderPanel.Position.Center
    layout(statusBar) = BorderPanel.Position.South
  } // def ui

  override def top = new MainFrame {
    title = applicationTitle
    iconImage = toolkit.getImage(getClass.getResource("resources/px-32ams-scala.png"))
    menuBar = ViewMenu.menuBar
    contents = ui()
    centerOnScreen()
  }
}

object Control {

  import TilesSolverApp._

  /** Place tiles in a grid */
  def displaySelected(solutions: Int, longestLen: Int, nAllLongestSolutions: Int, selTiles: Chain) {

    def placeTilesInGrid(toDraw: Map[Coord, (Tile, Int)], overlayPos: Set[Coord]): Panel = {
      // Compute the extremes, Most Top Left and the Most Bottom Right
      val (mostTopLeft, mostBottomRight) = TilesSolver.calculateExtremes(toDraw)
      new BoxPanel(Orientation.Vertical) {
        contents +=
          new GridPanel(1 + mostBottomRight._2 - mostTopLeft._2, 1 + mostBottomRight._1 - mostTopLeft._1) {
            contents ++= (for {
              y <- mostTopLeft._2 to mostBottomRight._2
              x <- mostTopLeft._1 to mostBottomRight._1
            } yield new Button {
                focusable = false
                minimumSize = dim
                preferredSize = dim

                val tile = toDraw.get(x, y)
                if (overlayPos.contains((x, y))) background = java.awt.Color.RED
                icon = if (tile.isDefined) {
                  tooltip = tile.get.toString()
                  getTileImage(tile.get._1)
                }
                else blancImg
              })
          }
        contents ++= List(VGlue)
      }
    } // def placeTilesInGrid

    middle.text = selTiles.mkString("\n")

    val tiles2dRaw = TilesSolver.virtualTilesLayouter(selTiles)
    val (tiles2D, overlayPos) = (tiles2dRaw.toMap, TilesSolver.findOverlayedPositions(tiles2dRaw))

    lblStatusField.text =
      s"Found $solutions potential solution(s), $nAllLongestSolutions are the longest, all $longestLen long, overlaps: ${overlayPos.size}."

    mainPanel.contents(4).visible = false // This does the trick of redraw the outputGrid
    mainPanel.contents(4) =
      if (longestLen == 0) {
        output.text = ""
        new BoxPanel(Orientation.Vertical) /*Clear text box*/
      }
      else {
        output.text = tiles2D.toList.sortBy { case (coord, tileWithSerialN) => tileWithSerialN._2}.
          map { case (coord, tileWithSerialN) => s"$coord ${tileWithSerialN._1}"}.mkString("\n")
        placeTilesInGrid(tiles2D, overlayPos)
      }
  } // def displaySelected

  def updateMiddle() {
    val solutions = TilesSolver.filterRealSolutions(Model.rawSolutions, !ViewMenu.chkNoOverlap.selected)
    val longestLen = solutions.foldLeft(0)(_ max _.size)

    given.text = Model.givenTiles.mkString("\n")

    ViewMenu.buildSolutionsMenu(solutions.size, longestLen, TilesSolver.allLongestSolutions(solutions))
    mainPanel.cursor = Cursor.getDefaultCursor
  }

}  // object Control

object TilesSolverApp extends View

