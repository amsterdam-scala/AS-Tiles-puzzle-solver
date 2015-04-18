package nl.amsscala
package tilessolver

import java.awt.print.Printable.{NO_SUCH_PAGE, PAGE_EXISTS}
import java.awt.print.{PageFormat, Printable, PrinterJob}
import java.awt.{Cursor, Graphics}
import javax.swing.ImageIcon

import scala.swing.Swing.{VGlue, pair2Dimension}
import scala.swing.{BorderPanel, BoxPanel, Button, Component, FlowPanel, GridPanel, Label,
                    MainFrame, Orientation, Panel, ScrollPane, SimpleSwingApplication, TextArea, event}

object Model {

  protected[tilessolver] var rawSolutions: Solution = Solution(Nil)

  private var givenTiles_ : Chain = Nil

  protected[tilessolver] def changeInput(tiles: Chain) {
    givenTiles_ = tiles
    TilesSolverApp.mainPanel.cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)
    TilesSolverApp.lblStatusField.text = s"Computing for ${givenTiles_.size} tiles ..."
    ////////////////////////////// processSituation /////////////////////////////

    rawSolutions = Solution(givenTiles)
    Control.updateCombination()
  }

  def givenTiles = givenTiles_
}

trait View extends SimpleSwingApplication {
  System.setProperty("apple.laf.useScreenMenuBar", "true")

  final val applicationTitle = "Tiles Puzzle Solver"
  final val applicationShort = "Tiles solver"
  final val blancImg = getImageByPartialPath("resources/TileXX.png")
  final val dim = (42, 42)

  val (given, combination, tessellation) =
    (new TextArea("Given tiles", 14, 10) {
      editable = false
    },
      new TextArea("Combination", 12, 10) {
        editable = false
      },
      new TextArea("Tessellation", 14, 16) {
        editable = false
      })
  val mainPanel = new BoxPanel(Orientation.Horizontal) with Printable {
    contents += tileBoard
    contents += new ScrollPane(given) {
      horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never
    }
    contents += new ScrollPane(combination) {
      horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never
    }
    contents += new ScrollPane(tessellation) {
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

  override def top = new MainFrame {
    title = applicationTitle
    iconImage = toolkit.getImage(getClass.getResource("resources/px-32ams-scala.png"))
    menuBar = ViewMenu.menuBar
    contents = ui()
    centerOnScreen()
  }

  private def ui(toolbar: Option[Component] = None) = new BorderPanel() {

    def statusBar = new FlowPanel(FlowPanel.Alignment.Leading)(lblStatusField)

    ///////////////////////// Start of ui //////////////////////////////////////

    if (toolbar.nonEmpty) add(toolbar.get, BorderPanel.Position.North)
    layout(mainPanel) = BorderPanel.Position.Center
    layout(statusBar) = BorderPanel.Position.South
  } // def ui

  private def tileBoard =
    new GridPanel(Directions.values.size, Directions.values.size) {

      import nl.amsscala.tilessolver.Directions.Directi

      def buttonsSeq = for {
        x <- Directions.values.view // Without view it would be in disorder
        y <- Directions.values.view
      } yield new Button {
          def butFingerprint(x: Directi, y: Directi): Option[Tile] = {
            val (tile, ico) = if (x == y) (None, blancImg) else (Option(Tile(x, y)), getTileImage(Tile(x, y)))
            tooltip = tile.getOrElse(None).toString
            icon = ico
            tile // Return tile
          } // def butFingerprint

          val tile = butFingerprint(x.asInstanceOf[Directi], y.asInstanceOf[Directi])
          minimumSize = dim
          preferredSize = minimumSize
          maximumSize = minimumSize

          listenTo(mouse.clicks)
          reactions += {
            case click: event.MouseClicked if tile.isDefined => Model.changeInput(Model.givenTiles ++ List(tile.get))
          }
        } // for yield & def buttonsSeq

      contents ++= buttonsSeq
    } // def tileBoard

  def getTileImage(tile: Tile) = getImageByPartialPath(s"resources/Tile${tile.start}${tile.end}.png")

  def getImageByPartialPath(partPath: String): ImageIcon =
    try {
      new ImageIcon(resourceFromClassloader(partPath))
    } catch {
      case t: Throwable =>
        println(s"$t: $partPath not found")
        throw new ExceptionInInitializerError("Resource not found")
    }

  object lblStatusField extends Label {
    text = ViewMenu.t("Compose a set of tiles by clicking on the tile pad.")
    horizontalAlignment = scala.swing.Alignment.Left
  }

} // trait View

object Control {
  import nl.amsscala.tilessolver.TilesSolverApp._

  /** Place tiles in a grid */
  def displaySelected(solutions: Int, longestLen: Int, nAllLongestSolutions: Int, selTiles: Chain) {

    def placeTilesInGrid(toDraw: Map[Coord, (Tile, Int)], overlayPos: Set[Coord]): Panel = {
      /** Compute the extremes, Lowest Bottom Left and the Most Top Right in one go */
      def calculateExtremes(toDraw: Map[Coord, (Tile, Int)]): (Coord, Coord) =
        toDraw.keys.foldLeft((toDraw.keys.head, toDraw.keys.head)) {
          case (((xLBL, yLBL), (xMTR, yMTR)), (x, y)) => ((xLBL min x, `yLBL` min y), (`xMTR` max x, `yMTR` max y))
        }

      // Compute the extremes, Lowest Bottom Left and the Most Top Right
      val ((xLBL, yLBL), (xMTR, yMTR)) = calculateExtremes(toDraw)

      new BoxPanel(Orientation.Vertical) {
        contents +=
          new GridPanel(1 + yMTR - yLBL, 1 + xMTR - xLBL) {
            contents ++= (for {y <- yLBL to yMTR; x <- xLBL to xMTR} yield new Button {
              focusable = false
              minimumSize = dim
              preferredSize = dim

              val tile = toDraw.get(x, y)
              if (overlayPos.contains((x, y))) background = java.awt.Color.RED
              icon = if (tile.isDefined) {
                tooltip = tile.get.toString()
                getTileImage(tile.get._1)
              } else blancImg
            })
          }
        contents ++= List(VGlue)
      }
    } // def placeTilesInGrid

    combination.text = selTiles.mkString("\n")

    val tiles2dRaw = Solution.virtualTilesLayouter(selTiles)
    val (tiles2D, overlayPos) = (tiles2dRaw.toMap, Solution.findOverlayedPositions(tiles2dRaw))

    lblStatusField.text =
      s"Found $solutions potential solution(s), $nAllLongestSolutions are the longest, all $longestLen long, overlaps: ${overlayPos.size}."

    mainPanel.contents(4).visible = false // This does the trick of redraw the outputGrid
    mainPanel.contents(4) =
      if (longestLen == 0) {
        tessellation.text = ""
        new BoxPanel(Orientation.Vertical) /*Clear text box*/
      }
      else {
        tessellation.text = tiles2D.toList.sortBy { case (coord, (tile, serialN)) => serialN}.
          map { case (coord, (tile, serialN)) => s"$coord $tile"}.mkString("\n")

        placeTilesInGrid(tiles2D, overlayPos)
      }
  } // def displaySelected

  def updateCombination() {

    given.text = Model.givenTiles.mkString("\n")

    ViewMenu.buildSolutionsMenu(Model.rawSolutions)

    mainPanel.cursor = Cursor.getDefaultCursor
  }

} // object Control

object TilesSolverApp extends View

