/**
  * Created by A599681 on 04/04/2017.
  */
case class Minesweeper(board: List[String], explored: Set[(Int, Int)] = Set.empty) {


  val rows: Int = board.length
  val cols: Int = board.headOption.map(_.length).getOrElse(0)

  def minesNear(row: Int, col: Int): Int = {
    for {
      r ← (row - 1 max 0) to ((row + 1) min (rows - 1))
      c ← (col - 1 max 0) to ((col + 1) min (cols - 1))
      if board(r)(c) == '*'
    } yield 1
  }.sum

  lazy val display: List[String] =
    List.tabulate(rows) { row ⇒
      Seq.tabulate(cols) { col ⇒
        if (explored(row, col)) solution(row)(col) else " "
      }.mkString
    }

  lazy val solution: List[String] = board.zipWithIndex.map { case (line, row) ⇒
    line.zipWithIndex.map {
      case ('*', _) ⇒ "*"
      case ('.', col) ⇒ minesNear(row, col)
    }.mkString
  }

  def explore(row: Int, col: Int): Option[Minesweeper] =
    if (board(row)(col) == '*') None
    else Some(copy(explored = explored + ((row, col))))

}
