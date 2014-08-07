package nl.amsscala
package tilessolver

import scala.swing.Swing.EmptyIcon
import scala.swing.{Action, Alignment, BorderPanel, Button, Dialog, GridBagPanel, Label, Swing}

protected class ViewHelp extends Dialog {
  title = s"Help - ${TilesSolverApp.applicationShort}"

  resizable = false
  this.peer.setIconImage(toolkit.getImage(getClass.getResource("resources/px-32ams-scala.png")))

  contents = new BorderPanel {
    layout(new GridBagPanel {
      add(new Label("",
        TilesSolverApp.getImageByPartialPath("resources/TilesRules.png"), Alignment.Center) {},
        new Constraints {
          /*grid =*/ (0, 0)
        })
    }) = BorderPanel.Position.Center
  }

  centerOnScreen()
  open()
}

protected class ViewAboutBox extends Dialog {
  title = s"About ${TilesSolverApp.applicationShort}"
  modal = true
  resizable = false

  contents = new BorderPanel {
    border = Swing.EmptyBorder(20, 20, 20, 20)

    layout(new GridBagPanel {
      private val gbc = new Constraints {
        //fill = scala.swing.GridBagPanel.Fill.Horizontal
        gridheight = 6
        ipadx = 12
        grid = (0, 0)
      }
      add(new Label("", EmptyIcon, Alignment.Center) {
        //name = ("imageLabel") // NOI18N
      }, gbc)
      gbc.gridheight = 1

      gbc.grid = (1, 0)
      add(new Label(TilesSolverApp.applicationTitle, EmptyIcon, Alignment.Left) {
        font = font.deriveFont(font.getStyle | java.awt.Font.BOLD, font.getSize + 4)
        //name = ("appTitleLabel") // NOI18N
      }, gbc)

      gbc.grid = (1, 5)
      add(new Label("<html>Dedicated to Fabio, who can find only<br>friends inside the Scala Meetup group :-)</html>",
        EmptyIcon, Alignment.Left) {
        //font = (font.deriveFont(font.getStyle() | java.awt.Font.BOLD))
        //name = ("homepageLabel") // NOI18N
      }, gbc)

      gbc.grid = (2, 0)
      add(new Label("",
        TilesSolverApp.getImageByPartialPath("resources/px128ams-scala.png"), Alignment.Trailing) {
        //name = ("appVersionLabel") // NOI18N
      }, gbc)
      /*
      gbc.grid = (2, 3)
      add(new Label("Application.vendor", EmptyIcon, Alignment.Left) {
        //name = ("appVendorLabel") // NOI18N
      }, gbc)
*/
      gbc.grid = (2, 4)
      add(new Label("Scala Amsterdam", EmptyIcon, Alignment.Left) {
        //name = ("appHomepageLabel") // NOI18N
      }, gbc)

      gbc.grid = (2, 5)
      gbc.fill = scala.swing.GridBagPanel.Fill.None
      add(new Button(Action("Close about Box") {
        dispose()
      }), gbc)
    }) = BorderPanel.Position.Center
  }
  centerOnScreen()
  open()
} // class ViewAboutBox