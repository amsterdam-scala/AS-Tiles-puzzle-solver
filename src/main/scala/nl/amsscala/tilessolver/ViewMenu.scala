package nl.amsscala
package tilessolver

import java.awt.event.KeyEvent
import java.awt.Toolkit
import javax.swing.{ ImageIcon, KeyStroke }

import scala.swing.{ AbstractButton, Action, Menu, MenuBar }
import scala.swing.{ MenuItem, Separator }
import scala.swing.Swing.EmptyIcon
import scala.swing.event.Key

object ViewMenu {
  private val shortcutKeyMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()

  def t(key: String) = key // Placeholder for resource manager

  private def mutateTextNmeIcon(
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

  private def menuItemFactory(
    pActionTitleResourceText: String,
    pActionBlock: => Unit,
    pAccelerator: Option[KeyStroke] = None,
    pIcon: javax.swing.Icon = EmptyIcon): MenuItem = {
    val comp = new MenuItem("")
    mutateTextNmeIcon(comp, pActionTitleResourceText, pActionBlock, pAccelerator, pIcon)
    comp
  }

  private val solutionsMenu = new Menu("") {
    mutateTextNmeIcon(this, "&Solutions")
    enabled = false
  }

  def buildSolutionsMenu(nSolution: Int,
                         longestLen: Int,
                         nAllLongestSolutions: Int,
                         solutions: Set[Chain]) = {
    solutionsMenu.enabled = solutions.size > 1
    solutionsMenu.contents.clear
    solutionsMenu.contents ++=
      (for (elem <- solutions.zipWithIndex) yield {
        menuItemFactory(t(s"Solution &${elem._2 + 1}"),
          {
            TilesSolverW.displaySelected(nSolution,
              longestLen,
              nAllLongestSolutions,
              TilesSolver.computeTilesIn2D(elem._1).toMap)
          })
      })

    TilesSolverW.displaySelected(nSolution,
      longestLen,
      nAllLongestSolutions,
      TilesSolver.computeTilesIn2D(solutions.headOption.getOrElse(Nil)).toMap)

  }

  def menuBar: MenuBar = new MenuBar {
    // File menu
    contents += new Menu("") {
      mutateTextNmeIcon(this, "&File")
      tooltip = "File Tooltip text"

      contents.append(
        menuItemFactory(t("&New"), { TilesSolverW.changeInput(Nil) },
          Some(KeyStroke.getKeyStroke(KeyEvent.VK_R, shortcutKeyMask))),
        menuItemFactory(t("mnuSaveItem.text"), {},
          Some(KeyStroke.getKeyStroke(KeyEvent.VK_S, shortcutKeyMask))),
        menuItemFactory(t("mnuSaveAsItem.text"), {}),
        new Separator,
        menuItemFactory(
          s"${t("E&xit")} ${TilesSolverW.applicationShort}",
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
      enabled = false
    }

    import Directions._
    // Tiles menu
    contents += new Menu("") {
      mutateTextNmeIcon(this, "&Tiles")
      contents.append(menuItemFactory(t("Asse&gnazione originale di Fabio"),
        TilesSolverW.changeInput(TilesSolver.fabioPhoto),
        Some(KeyStroke.getKeyStroke(KeyEvent.VK_1, shortcutKeyMask)) //
        , //
        new ImageIcon(getClass.getResource("resources/px-20ticofab.png"))),
        menuItemFactory(t("&Missing tiles due to overlaps"),
          TilesSolverW.changeInput(List(Tile(C, E), Tile(W, S), Tile(N, W), Tile(E, N), Tile(S, C))),
          Some(KeyStroke.getKeyStroke(KeyEvent.VK_2, shortcutKeyMask))))
    }

    // Solutions menu
    contents += solutionsMenu

    // Window menu
    contents += new Menu("") { mutateTextNmeIcon(this, "&Window"); enabled = false }

    /*val mnuShowToolBar = new CheckMenuItem("")
      mutateTextNmeIcon(mnuShowToolBar,
        "showToolBar.Action.text",
        { OXO_View.toolBar.visible = mnuShowToolBar.selected },
        Some(KeyStroke.getKeyStroke(KeyEvent.VK_T, OXO_GUI.shortcutKeyMask)))
      contents += mnuShowToolBar
      mnuShowToolBar.selected = true
    }*/

    // Help Menu
    contents += new Menu("") {
      mutateTextNmeIcon(this, "&Help")

      contents += menuItemFactory(
        t("Show &help"), { new ViewHelp }, Some(KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0)))

      contents += menuItemFactory(t("&About"), new ViewAboutBox)
    }
    tooltip = "Menubar tooltip text"
  } // def menuBar
}