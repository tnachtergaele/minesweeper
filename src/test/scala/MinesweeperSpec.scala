import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks

import PathObject._

@RunWith(classOf[JUnitRunner])
class MinesweeperSpec extends FlatSpec with Matchers {
  val emptyMinesweeper = Minesweeper(List.empty[String])

  "Empty Minesweeper" should "have empty representation" in {
    emptyMinesweeper.solution shouldBe List.empty[String]
  }

  "Minesweeper with only free cells" should "have representation with only zeroes" in {
    val minesweeper = Minesweeper(List.fill(2)("..."))
    minesweeper.solution shouldBe List.fill(2)("000")
  }

  "Empty Minesweeper" should "compute nearby mines" in {
    emptyMinesweeper.minesNear(0, 0) shouldBe 0
  }

  "Minesweeper with only free cells" should "compute nearby mines" in {
    val minesweeper = Minesweeper(List.fill(2)("..."))
    emptyMinesweeper.minesNear(0, 0) shouldBe 0
  }

  val minesweeperSimple = Minesweeper(List(
    "...",
    ".*.",
    "..."))

  "Minesweeper with one mine" should "compute nearby mines" in {
    minesweeperSimple.minesNear(0, 0) shouldBe 1
  }

  "Minesweeper with one mine" should "compute its solution" in {
    minesweeperSimple.solution shouldBe List(
      "111",
      "1*1",
      "111")
  }

  "Minesweeper example 1" should "compute its solution" in {
    val minesweeper = Minesweeper(List(
      "*...",
      "....",
      ".*..",
      "...."))
    minesweeper.solution shouldBe List(
      "*100",
      "2210",
      "1*10",
      "1110")
  }

  "Minesweeper example 2" should "compute its solution" in {
    val minesweeper = Minesweeper(List(
      "**...",
      ".....",
      ".*..."))
    minesweeper.solution shouldBe List(
      "**100",
      "33200",
      "1*100")
  }

  "Minesweeper with one mine" should "initially display to player" in {
    minesweeperSimple.display shouldBe List(
      "   ",
      "   ",
      "   ")
  }

  "Minesweeper with one mine" should "record exploration move" in {
    minesweeperSimple.explore(0,0).get.display shouldBe List(
      "1  ",
      "   ",
      "   ")
  }

  "Minesweeper with one mine" should "be none when explore the mine" in {
    minesweeperSimple.explore(1,1) shouldBe None
  }

  "Minesweeper example 2" should "display partially explored board" in {
    val minesweeper = Minesweeper(List(
      "**...",
      ".....",
      ".*..."))
    minesweeper.explore(0,2).get.explore(1,1).get.display shouldBe List(
      "  1  ",
      " 3   ",
      "     ")
  }

}
