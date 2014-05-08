package nl.amsscala.tilessolver

import java.awt.event.KeyEvent

import scala.swing.MenuBar
import scala.swing.Menu
import scala.swing.AbstractButton
import scala.swing.CheckMenuItem
import scala.swing.Action
import scala.swing.event.Key
import scala.swing.Swing.EmptyIcon
import scala.swing.Separator
import scala.swing.MenuItem
import javax.swing.KeyStroke

object ViewMenu {
  private val AMPERSAND = '&'

  def t(key: String) = key

  private def menuItemFactory(
    pActionTitleResourceText: String,
    pActionBlock: => Unit,
    pAccelerator: Option[javax.swing.KeyStroke] = None,
    pIcon: javax.swing.Icon = EmptyIcon): MenuItem =
    {
      val comp = new MenuItem("")
      mutateTextNmeIcon(comp, pActionTitleResourceText, pActionBlock, pAccelerator, pIcon)
      comp
    }

  def mutateTextNmeIcon(
    pComp: AbstractButton,
    pActionTitleResourceText: String,
    pActionBlock: => Unit = {},
    pAccelerator: Option[javax.swing.KeyStroke] = None,
    pIcon: javax.swing.Icon = EmptyIcon) {

    // The mnemonic parser
    var ampFlag = false
    var mne: Option[Char] = None
    pComp.action = Action(t(pActionTitleResourceText).filter(
      (c: Char) => { // The ampersand filter evaluator
        val isStringText = (c != AMPERSAND) || ampFlag // The last term is for && (escape)
        if (ampFlag) {
          if (c != AMPERSAND) mne = Some(c)
          ampFlag = false // destructive assignment
        } else ampFlag = (c == AMPERSAND)
        isStringText
      } //menuMne.sifter(_))
      )) { pActionBlock }
    // Mutate component
    if (!mne.isEmpty) pComp.mnemonic = Key.withName((mne.get).toUpper.toString)
    pComp.icon = pIcon
    pComp.action.accelerator = pAccelerator
  }

  def menuBar = new MenuBar {

    tooltip = "Menubar tooltip text"
    // File menu
    private val mnuSaveItem = {
      menuItemFactory("mnuSaveItem.text", {},
        Some(KeyStroke.getKeyStroke(KeyEvent.VK_S, TilesSolverW.shortcutKeyMask)),
        EmptyIcon)
    }
    private val mnuSaveAsItem = {
      menuItemFactory("mnuSaveAsItem.text", {}, None, EmptyIcon)
    }

    contents += new Menu("") {
      mutateTextNmeIcon(this, "File")
      tooltip = "Tooltip text"

      contents.append(mnuSaveItem, mnuSaveAsItem, new Separator,
        new Separator, /*mnuResetGameItem,*/
        menuItemFactory(
          "exitMenuItem.text",
          { sys.exit }, None /*,
          new ImageIcon(getClass.getResource(('/' + RESOURCEPATH + "images/px-16gnome_application_exit.png")))*/ ))
    }

    // Edit menu
    contents += new Menu("") {
      mutateTextNmeIcon(this, "Edit")
      //      contents.append(OXO_ViewMenu.mnuUndoItem, OXO_ViewMenu.mnuRedoItem, OXO_ViewMenu.mnuClearBoardItem)
    }

    // View menu
    contents += new Menu("") {
      mutateTextNmeIcon(this, "View")

    }

    // Window menu
    contents += new Menu("") {
      mutateTextNmeIcon(this, "Window")
    }
    /*      val mnuShowToolBar = new CheckMenuItem("")
      mutateTextNmeIcon(mnuShowToolBar,
        "showToolBar.Action.text",
        { OXO_View.toolBar.visible = mnuShowToolBar.selected },
        Some(KeyStroke.getKeyStroke(KeyEvent.VK_T, OXO_GUI.shortcutKeyMask)))
      contents += mnuShowToolBar
      mnuShowToolBar.selected = true
    }
*/
    // Help Menu
    contents += new Menu("") {
      mutateTextNmeIcon(this, "Help")

      contents += menuItemFactory(
        "showHelpBox.Action.text",
        {},
        Some(KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0)))

      contents += menuItemFactory(
        "About",
        new ViewAboutBox,
        None)
    }
  } // def menuBar
}