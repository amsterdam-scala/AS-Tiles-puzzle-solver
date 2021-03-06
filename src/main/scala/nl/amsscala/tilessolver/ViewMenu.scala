package nl.amsscala
package tilessolver

import java.awt.Toolkit
import java.awt.event.KeyEvent
import javax.swing.KeyStroke

import scala.language.reflectiveCalls
import scala.swing.Swing.{EmptyIcon, HGlue}
import scala.swing.event.Key
import scala.swing.{AbstractButton, Action, CheckBox, Menu, MenuBar, MenuItem, Separator}


trait MenuUtils {

  def t(key: String) = key // Placeholder for resource manager

  protected def mutateTextNmeIcon(pComp: AbstractButton,
                                  pActionTitleResourceText: String,
                                  pActionBlock: => Unit = {},
                                  pAccelerator: Option[KeyStroke] = None,
                                  pIcon: javax.swing.Icon = EmptyIcon) {

    /** The mnemonic character is the first character after an ampersand character (&) in the text
      * of the MenuItem. This function will skip a mnemonic if two ampersand characters are
      * placed together as this will be used to display an ampersand in the text of the MenuItem
      * instead of defining a mnemonic character.
      * The function returns a mnemonic character/text pair.
      */
    val (mne, mnuItemText) = {
      val text = t(pActionTitleResourceText)
      // The mnemonic parser, filter by the mnemonic character(s) and text
      (text.replaceAll("&&", "").sliding(2).filter(_.init == "&").take(1), // Mnemonic member of pair
        "&[^&]".r.replaceAllIn(text, _.matched.tail)) // Item text part of pair
    }

    pComp.action = Action(mnuItemText) {
      pActionBlock
    }

    // Mutate component
    if (mne.hasNext) pComp.mnemonic = Key.withName(mne.next().last.toUpper.toString)
    if (!pComp.isInstanceOf[CheckBox]) pComp.icon = pIcon
    pComp.action.accelerator = pAccelerator
  } // def mutateTextNmeIcon

  protected def menuItemFactory(pActionTitleResourceText: String,
                                pActionBlock: => Unit,
                                pAccelerator: Option[KeyStroke] = None,
                                pIcon: javax.swing.Icon = EmptyIcon): MenuItem = {
    new MenuItem("") {
      mutateTextNmeIcon(this, pActionTitleResourceText, pActionBlock, pAccelerator, pIcon)
    }
  }
}

// trait MenuUtils

object ViewMenu extends MenuUtils {
  private final val shortcutKeyMask = Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  private val solutionsMenu = new Menu("") {
    mutateTextNmeIcon(this, t("&Solutions"))
    enabled = false
  }

  private def chkSorted = new CheckBox() {
    mutateTextNmeIcon(this, t("Sorted &lists"))
    selected = true
    tooltip = t("Nothing fancy to do with this")
  }

  private val chkNoOverlap = new CheckBox() {
    action = Action(t("No overlaps")) {
      Control.updateCombination()
    }
  }

  def buildSolutionsMenu(solutions: Solution) {
    val nSolutions = solutions.rawSolution.size /*Total number of all solutions small and long*/
    val sol = solutions.allLongestSolutions(ViewMenu.chkNoOverlap.selected)
    val allLength = sol.headOption.map(_.size).getOrElse(0)
    solutionsMenu.enabled = sol.size > 1
    solutionsMenu.contents.clear()
    solutionsMenu.contents ++= (for ((chain, numberOfChain) <- sol.zipWithIndex.view) yield {
      menuItemFactory(t(s"Solution &${numberOfChain + 1}"),
        Control.displaySelected(nSolutions, allLength, sol.size, chain)) // Action
    })

    Control.displaySelected(nSolutions, allLength, sol.size, sol.headOption.getOrElse(Nil))
  }

  def menuBar: MenuBar = new MenuBar {
    contents.append(new Menu("") {
      // File menu
      mutateTextNmeIcon(this, t("&File"))
      tooltip = "File Tooltip text"

      contents.append(
        menuItemFactory(t("&New"),
          Model.changeInput(Nil),
          Some(KeyStroke.getKeyStroke(KeyEvent.VK_R, shortcutKeyMask))),
        new Separator,
        menuItemFactory(t("&Print"), TilesSolverApp.mainPanel.doPrint(),
          Some(KeyStroke.getKeyStroke(KeyEvent.VK_P, shortcutKeyMask))),
        new Separator,
        menuItemFactory(
          s"${t("E&xit")} ${TilesSolverApp.applicationShort}", sys.exit(), None,
          TilesSolverApp.getImageByPartialPath("/resources/px-16gnome_application_exit.png")))
    }, new Menu("") {
      // Edit menu
      mutateTextNmeIcon(this, t("&Edit"))
      enabled = false
    }, new Menu("") {
      // View menu
      mutateTextNmeIcon(this, t("&View"))
      contents.append(chkSorted)
    },
      new Menu("") {
        // Tiles menu
        mutateTextNmeIcon(this, t("&Tiles"))
        contents.append(menuItemFactory(t("Asse&gnazione originale di Fabio"),
          Model.changeInput(Suggestions.fabioPhoto),
          Some(KeyStroke.getKeyStroke(KeyEvent.VK_1, shortcutKeyMask)),
          TilesSolverApp.getImageByPartialPath("resources/px-20ticofab.png")),
          menuItemFactory(t("&Missing tiles due to overlaps"), Model.changeInput(Suggestions.missingsTileOverlap),
            Some(KeyStroke.getKeyStroke(KeyEvent.VK_2, shortcutKeyMask))),
          menuItemFactory(t("&Modified example"), Model.changeInput(Suggestions.modifiedExample),
            Some(KeyStroke.getKeyStroke(KeyEvent.VK_2, shortcutKeyMask))),
          menuItemFactory(t("C&razy example"), Model.changeInput(Suggestions.crazyExample),
            Some(KeyStroke.getKeyStroke(KeyEvent.VK_3, shortcutKeyMask))))
      }, solutionsMenu, chkNoOverlap, HGlue,
      new Menu("") {
        // Window menu
        mutateTextNmeIcon(this, t("&Window"))
        enabled = false
      },
      new Menu("") {
        // Help Menu
        mutateTextNmeIcon(this, t("&Help"))
        contents.append(menuItemFactory(
          t("Show &help"), new ViewHelp, Some(KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0))),
          menuItemFactory(t("&About"), new ViewAboutBox))
      })
  } // def menuBar

}

// object ViewMenu