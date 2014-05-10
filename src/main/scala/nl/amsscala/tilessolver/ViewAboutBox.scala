package nl.amsscala.tilessolver

import scala.swing.{ Action, Alignment, BorderPanel, Button, Dialog }
import scala.swing.{ GridBagPanel, Label, Swing }
import scala.swing.Swing.EmptyIcon

class ViewAboutBox extends Dialog {

  title = ("About") // NOI18N
  modal = true
  //resizable = false

  contents = new BorderPanel {
    border = Swing.EmptyBorder(20, 20, 20, 20)

    layout(new GridBagPanel {
      private val gbc = new Constraints {
        fill = scala.swing.GridBagPanel.Fill.Horizontal
        gridheight = 6
        ipadx = 12
        grid = (0, 0)
      }
      add(new Label("", EmptyIcon, Alignment.Center) {
        //name = ("imageLabel") // NOI18N
      }, gbc)
      gbc.gridheight = 1

      gbc.grid = (1, 0)
      add(new Label(TilesSolverW.applicationTitle, EmptyIcon, Alignment.Left) {
        font = (font.deriveFont(font.getStyle() | java.awt.Font.BOLD, font.getSize() + 4));
        //name = ("appTitleLabel") // NOI18N
      }, gbc)

      gbc.grid = (1, 1)
      add(new Label("Dedicated to Fabio, who cannot find friends outside the Scala Meetup group :-)", EmptyIcon, Alignment.Left) {
        //name = ("appDescLabel") // NOI18N
      }, gbc)

      /*      gbc.grid = (1, 2)
      add(new Label("versionLabel.text", EmptyIcon, Alignment.Left) {
        font = (font.deriveFont(font.getStyle() | java.awt.Font.BOLD));
        //name = ("versionLabel") // NOI18N
      }, gbc)

      gbc.grid = (1, 3)
      add(new Label("vendorLabel.text", EmptyIcon, Alignment.Left) {
        font = (font.deriveFont(font.getStyle() | java.awt.Font.BOLD))
        //name = ("vendorLabel") // NOI18N
      }, gbc)

      gbc.grid = (1, 4)
      add(new Label("homepageLabel.text", EmptyIcon, Alignment.Left) {
        font = (font.deriveFont(font.getStyle() | java.awt.Font.BOLD))
        //name = ("homepageLabel") // NOI18N
      }, gbc)

      gbc.grid = (2, 2)
      add(new Label("Application.version", EmptyIcon, Alignment.Left) {
        //name = ("appVersionLabel") // NOI18N
      }, gbc)

      gbc.grid = (2, 3)
      add(new Label("Application.vendor", EmptyIcon, Alignment.Left) {
        //name = ("appVendorLabel") // NOI18N
      }, gbc)

      gbc.grid = (2, 4)
      add(new Label("Application.homepage", EmptyIcon, Alignment.Left) {
        //name = ("appHomepageLabel") // NOI18N
      }, gbc)
*/
      gbc.grid = (2, 5)
      gbc.fill = scala.swing.GridBagPanel.Fill.None
      add(new Button(Action("Close about Box") { dispose() }), gbc)
    }) = BorderPanel.Position.Center
  }
  visible = true
}