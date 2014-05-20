package nl.amsscala
package tilessolver

import java.awt.event.KeyEvent
import java.awt.Toolkit
import javax.swing.{ ImageIcon, KeyStroke }
import scala.swing.{ AbstractButton, Action, CheckBox, Menu, MenuBar }
import scala.swing.{ MenuItem, Separator }
import scala.swing.Swing.{ EmptyIcon, HGlue }
import scala.swing.event.Key

trait MenuUtils {

  def t(key: String): String

  def mutateTextNmeIcon(
    pComp: AbstractButton,
    pActionTitleResourceText: String,
    pActionBlock: => Unit = {},
    pAccelerator: Option[KeyStroke] = None,
    pIcon: javax.swing.Icon = EmptyIcon) {

    /** The mnemonic character is the first character after an ampersand character (&) in the text
     *  of the MenuItem. This function will not return a mnemonic if two ampersand characters are
     *  placed together as the ampersands are used to display an ampersand in the text of the
     *  MenuItem instead of defining a mnemonic character.
     *  The function returns a kind of mnemonic character / text pair.
     */
    val (mne, mnuItemText) = {
      val text = t(pActionTitleResourceText)
      // The mnemonic parser, filter the mnemonic character(s)
      (text.replaceAll("&&", "").sliding(2).filter(_.init == "&").take(1),
        "&[^&]".r.replaceAllIn(text, m => m.matched.tail)) // Filter the ampersands
    }

    pComp.action = Action(mnuItemText) { pActionBlock }

    // Mutate component
    if (mne.hasNext) pComp.mnemonic = Key.withName((mne.next.last).toUpper.toString)
    pComp.icon = pIcon
    pComp.action.accelerator = pAccelerator
  }

  def menuItemFactory(
    pActionTitleResourceText: String,
    pActionBlock: => Unit,
    pAccelerator: Option[KeyStroke] = None,
    pIcon: javax.swing.Icon = EmptyIcon): MenuItem = {
    new MenuItem("") { mutateTextNmeIcon(this, pActionTitleResourceText, pActionBlock, pAccelerator, pIcon) }
  }

  /*  case class MenuEx() extends AbstractButton{
    def apply = new Menu("") 
  }
*/
} // trait MenuUtils

object ViewMenu extends MenuUtils {
  private val shortcutKeyMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()

  def t(key: String) = key // Placeholder for resource manager

  private val solutionsMenu = new Menu("") {
    mutateTextNmeIcon(this, "&Solutions")
    enabled = false
  }

  val chkSorted = new CheckBox("Sorted lists") { selected = true }

  val chkNoOverlap = new CheckBox() { action = Action("No overlaps") { TilesSolverApp.updateMiddle() } }

  def buildSolutionsMenu(nSolution: Int,
                         longestLen: Int,
                         nAllLongestSolutions: Int,
                         solutions: Set[Chain]) = {
    solutionsMenu.enabled = solutions.size > 1
    solutionsMenu.contents.clear
    solutionsMenu.contents ++=
      (for (elem <- solutions.zipWithIndex.view) yield {
        menuItemFactory(t(s"Solution &${elem._2 + 1}"),
          TilesSolverApp.displaySelected(nSolution,
            longestLen,
            nAllLongestSolutions,
            elem._1))
      })

    TilesSolverApp.displaySelected(nSolution,
      longestLen,
      nAllLongestSolutions,
      solutions.headOption.getOrElse(Nil))
  }

  def menuBar: MenuBar = new MenuBar {
    // File menu
    contents += new Menu("") {
      mutateTextNmeIcon(this, "&File")
      tooltip = "File Tooltip text"

      contents.append(
        menuItemFactory(t("&New"), { TilesSolverApp.changeInput(Nil) },
          Some(KeyStroke.getKeyStroke(KeyEvent.VK_R, shortcutKeyMask))),
        new Separator,
        menuItemFactory(t("&Print"), { TilesSolverApp.mainPanel.doPrint },
          Some(KeyStroke.getKeyStroke(KeyEvent.VK_P, shortcutKeyMask))),
        new Separator,
        menuItemFactory(
          s"${t("E&xit")} ${TilesSolverApp.applicationShort}",
          { sys.exit }, None,
          new ImageIcon(getClass.getResource("/resources/px-16gnome_application_exit.png"))))
    }

    // Edit menu
    contents += new Menu("") {
      mutateTextNmeIcon(this, "&Edit")
      enabled = false
    }

    // View menu
    contents += new Menu("") {
      mutateTextNmeIcon(this, "&View")
      //enabled = false
      contents.append(chkSorted)
    }

    import Directions.{ C, N, E, S, W }
    // Tiles menu
    contents += new Menu("") {
      mutateTextNmeIcon(this, "&Tiles")
      contents.append(menuItemFactory(t("Asse&gnazione originale di Fabio"),
        TilesSolverApp.changeInput(TilesSolver.fabioPhoto),
        Some(KeyStroke.getKeyStroke(KeyEvent.VK_1, shortcutKeyMask)) //
        , //
        new ImageIcon(getClass.getResource("resources/px-20ticofab.png"))),
        menuItemFactory(t("&Missing tiles due to overlaps"),
          TilesSolverApp.changeInput(List(Tile(C, E), Tile(N, S), Tile(S, N), Tile(W, S), Tile(N, W), Tile(E, N), Tile(S, C))),
          Some(KeyStroke.getKeyStroke(KeyEvent.VK_2, shortcutKeyMask))),
        menuItemFactory(t("&Modified example"),
          TilesSolverApp.changeInput(TilesSolver.modifiedExample),
          Some(KeyStroke.getKeyStroke(KeyEvent.VK_2, shortcutKeyMask))),
        menuItemFactory(t("C&razy example"),
          TilesSolverApp.changeInput(TilesSolver.crazyExample),
          Some(KeyStroke.getKeyStroke(KeyEvent.VK_3, shortcutKeyMask))))
    }

    // Solutions menu
    contents += solutionsMenu

    contents += chkNoOverlap

    contents += HGlue

    // Window menu
    contents += new Menu("") { mutateTextNmeIcon(this, "&Window"); enabled = false }

    // Help Menu
    contents += new Menu("") {
      mutateTextNmeIcon(this, "&Help")

      contents += menuItemFactory(
        t("Show &help"), { new ViewHelp }, Some(KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0)))

      contents += menuItemFactory(t("&About"), new ViewAboutBox)
    }
  } // def menuBar
}