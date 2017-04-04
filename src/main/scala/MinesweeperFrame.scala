import scala.swing._
import scala.swing.event._
import scala.util.Random

object MinesweeperFrame extends SimpleSwingApplication {

  val initial = Minesweeper(List.fill(7) {
    Seq.fill(8)(if (Random.nextInt(5) == 0) '*' else '.').mkString
  })


  def top = new MainFrame {
    title = "Minesweeper"
    contents = updatePanel(initial)
  }

  def updatePanel(minesweeper: Minesweeper): Panel =
    new GridPanel(minesweeper.rows, minesweeper.cols) {
      println(minesweeper.display.mkString("\n"))
      for {
        r ← 0 until minesweeper.rows
        c ← 0 until minesweeper.cols
      } contents += buildCellLabel(minesweeper, r, c)
    }

  def buildCellLabel(minesweeper: Minesweeper, r: Int, c: Int) = new Label(minesweeper.display(r)(c).toString) {
    listenTo(mouse.clicks)
    reactions += {
      case _: MousePressed ⇒
        minesweeper.explore(r, c) match {
          case Some(m) ⇒
            text = m.display(r)(c).toString
            top.repaint()
          case None ⇒
            println("dead")
            println(minesweeper.solution.mkString("\n"))
        }
    }
  }
}
