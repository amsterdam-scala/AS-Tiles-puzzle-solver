package nl.amsscala
package tilessolver

import java.awt.event.KeyEvent
import java.awt.Toolkit
import javax.swing.{ ImageIcon, KeyStroke }
import scala.swing.{ AbstractButton, Action, CheckBox }
import scala.swing.{ Menu, MenuBar, MenuItem, Separator }
import scala.swing.Swing.{ EmptyIcon, HGlue }
import scala.swing.event.Key

trait MenuUtils {

  def t(key: String) = key // Placeholder for resource manager

  protected def mutateTextNmeIcon(
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
      // The mnemonic parser, filter by the mnemonic character(s) and text
      (text.replaceAll("&&", "").sliding(2).filter(_.init == "&").take(1),
        "&[^&]".r.replaceAllIn(text, _.matched.tail)) // Filter the ampersands
    }

    pComp.action = Action(mnuItemText) { pActionBlock }

    // Mutate component
    if (mne.hasNext) pComp.mnemonic = Key.withName((mne.next.last).toUpper.toString)
    if (!pComp.isInstanceOf[CheckBox]) pComp.icon = pIcon
    pComp.action.accelerator = pAccelerator
  }

  protected def menuItemFactory(
    pActionTitleResourceText: String,
    pActionBlock: => Unit,
    pAccelerator: Option[KeyStroke] = None,
    pIcon: javax.swing.Icon = EmptyIcon): MenuItem = {
    new MenuItem("") { mutateTextNmeIcon(this, pActionTitleResourceText, pActionBlock, pAccelerator, pIcon) }
  }
} // trait MenuUtils

object ViewMenu extends MenuUtils {
  private val shortcutKeyMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()
  private val solutionsMenu = new Menu("") { mutateTextNmeIcon(this, t("&Solutions")); enabled = false }

  private val chkSorted = new CheckBox() {
    mutateTextNmeIcon(this, t("Sorted &lists")); selected = true; tooltip = t("Nothing fancy to do with this")
  }

  val chkNoOverlap = new CheckBox() { action = Action(t("No overlaps")) { Control.updateMiddle() } }

  def buildSolutionsMenu(nSolution: Int, longestLen: Int, nAllLongestSolutions: Int, solutions: Set[Chain]) = {
    solutionsMenu.enabled = solutions.size > 1
    solutionsMenu.contents.clear
    solutionsMenu.contents ++=
      (for (elem <- solutions.zipWithIndex.view) yield {
        menuItemFactory(t(s"Solution &${elem._2 + 1}"),
          Control.displaySelected(nSolution, longestLen, nAllLongestSolutions, elem._1))
      })

    Control.displaySelected(nSolution, longestLen, nAllLongestSolutions, solutions.headOption.getOrElse(Nil))
  }

  def menuBar: MenuBar = new MenuBar {
    // File menu
    contents += new Menu("") {
      mutateTextNmeIcon(this, t("&File"))
      tooltip = "File Tooltip text"

      contents.append(
        menuItemFactory(t("&New"), { Model.changeInput(Nil) },
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
      mutateTextNmeIcon(this, t("&Edit"))
      enabled = false
    }

    // View menu
    contents += new Menu("") {
      mutateTextNmeIcon(this, t("&View"))
      //enabled = false
      contents.append(chkSorted)
    }

    // Tiles menu
    import Directions.{ C, N, E, S, W }
    contents += new Menu("") {
      mutateTextNmeIcon(this, t("&Tiles"))
      contents.append(menuItemFactory(t("Asse&gnazione originale di Fabio"),
        Model.changeInput(TilesSolver.fabioPhoto),
        Some(KeyStroke.getKeyStroke(KeyEvent.VK_1, shortcutKeyMask)) //
        , //
        new ImageIcon(getClass.getResource("resources/px-20ticofab.png"))),
        menuItemFactory(t("&Missing tiles due to overlaps"),
          Model.changeInput(TilesSolver.missingsTileOverlap),
          Some(KeyStroke.getKeyStroke(KeyEvent.VK_2, shortcutKeyMask))),
        menuItemFactory(t("&Modified example"),
          Model.changeInput(TilesSolver.modifiedExample),
          Some(KeyStroke.getKeyStroke(KeyEvent.VK_2, shortcutKeyMask))),
        menuItemFactory(t("C&razy example"),
          Model.changeInput(TilesSolver.crazyExample),
          Some(KeyStroke.getKeyStroke(KeyEvent.VK_3, shortcutKeyMask))))
    }

    // Solutions menu
    contents += solutionsMenu

    contents += chkNoOverlap

    contents += HGlue

    // Window menu
    contents += new Menu("") { mutateTextNmeIcon(this, t("&Window")); enabled = false }

    // Help Menu
    contents += new Menu("") {
      mutateTextNmeIcon(this, t("&Help"))

      contents += menuItemFactory(
        t("Show &help"), { new ViewHelp }, Some(KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0)))

      contents += menuItemFactory(t("&About"), new ViewAboutBox)
    }
  } // def menuBar
} // object ViewMenu 