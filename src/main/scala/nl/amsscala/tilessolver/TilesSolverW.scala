package nl.amsscala.tilessolver

import java.awt.Color
import java.awt.Dimension
import scala.swing.{ BorderPanel, BoxPanel, Button, Component }
import scala.swing.{ GridPanel, Label, MainFrame, Orientation }
import scala.swing.{ ScrollPane, SimpleSwingApplication, TextArea, event }
import javax.swing.ImageIcon

object TilesSolverW extends SimpleSwingApplication {
  private val dim = new Dimension(42, 42)

  import Directions._

  //  val boardSize = Directions.values.size
  //  val linearIndexRange = 0 until boardSize * boardSize
  val blancImg = new ImageIcon(resourceFromClassloader("resources/TileXX.png"))

  def getImage(tile: Tile) = {
    new ImageIcon(resourceFromClassloader(s"resources/Tile${tile.start}${tile.end}.png"))
  }

  def ui(toolbar: Option[Component] = None) = new BorderPanel() {

    var givenTiles: TilesToUse = Nil

    val given = new TextArea(5, 20) { editable = false }
    val output = new TextArea(5, 20) { editable = false }

    def outputGrid(toDraw: Map[(Int, Int), Tile]): GridPanel = {

      val extremes = toDraw.keys.foldLeft((toDraw.keys.head, toDraw.keys.head)) {
        (accu, tileCoord) =>
          ((accu._1._1 min tileCoord._1, accu._1._2 min tileCoord._2),
            (accu._2._1 max tileCoord._1, accu._2._2 max tileCoord._2))
      }
      println(toDraw)
      println(extremes)

      val min = extremes._1
      val max = extremes._2

      //      println(s"Size: ${drawIn.columns}, ${drawIn.rows}, Min: $min max: $max")

      new GridPanel(1 + max._2 - min._2, 1 + max._1 - min._1) {
        contents ++= {
          for (
            y <- (min._2 to max._2);
            x <- min._1 to max._1
          ) yield new Label {

            val tile = toDraw.get((x, y))
            //            println(s"Lookup: ${x} ${y} $tile")
            icon = if (tile.isDefined) getImage(tile.get) else blancImg
          }
        }

        //        println(drawIn.contents.length)
      }
    }

    //    drawTwoDin(Map((1, 0) -> Tile(C, S)), outGrid)

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

        minimumSize = dim
        preferredSize = minimumSize
        maximumSize = minimumSize

        val tile = butFingerprint(x.asInstanceOf[Directi], y.asInstanceOf[Directi])
        listenTo(mouse.clicks)
        reactions += {
          case me: event.MouseClicked => {

            if (tile.isDefined) givenTiles ++= List(tile.get)
            given.text = s"$givenTiles\n"

            val result = TilesSolver.findChains(givenTiles)
            val longestLen = result.foldLeft(0)(_ max _.size)

            val twoDim = TwoDim.toDim(result.filter(_.length >= longestLen).headOption.getOrElse(Nil))

            output.text_=(twoDim.mkString("\n"))
            if (!twoDim.isEmpty) {
              /*Swing.onEDT*/ ({
                mainPanel.contents(3).visible = false // This does the trick of redraw the outputGrid
                mainPanel.contents.remove(3)
                mainPanel.contents += outputGrid(twoDim)
              })
            }

          } // event.MouseClicked

        }
      } // for yield
    }

    def tileBoard: Component =
      new GridPanel(Directions.values.size, Directions.values.size) { contents ++= buttonsSeq }

    val mainPanel = new BoxPanel(Orientation.Vertical) {
      contents += tileBoard
      contents += new ScrollPane(given)
      contents += new ScrollPane(output)
      contents += outputGrid(Map((1, 0) -> Tile(C, S)))
    }

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