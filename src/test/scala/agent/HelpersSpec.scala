package agent

import org.scalatest.wordspec._
import Helpers._
import main.Player._
import main.Board
import main.Point

class AgentSpec extends AnyWordSpec {
  "the eye determiner method" should {
    "not deem a filled point an eye" in {
      val board = new Board(3, 3)
      board.placeStone(Black, new Point(1, 1))
      assert(!isPointAnEye(board, new Point(1, 1), Black))
    }
    "identify an eye correctly" in {
      val board = new Board(5, 5)
      board.placeStone(Black, new Point(1, 1))
      board.placeStone(White, new Point(1, 2))
      board.placeStone(Black, new Point(2, 1))
      board.placeStone(White, new Point(2, 2))
      board.placeStone(Black, new Point(3, 2))
      board.placeStone(White, new Point(2, 3))
      board.placeStone(Black, new Point(3, 3))
      board.placeStone(White, new Point(1, 4))
      board.placeStone(Black, new Point(3, 4))
      board.placeStone(White, new Point(2, 4))
      board.placeStone(Black, new Point(3, 5))
      board.placeStone(White, new Point(2, 5))
      assert(isPointAnEye(board, new Point(1, 3), White))
    }
  }
}
