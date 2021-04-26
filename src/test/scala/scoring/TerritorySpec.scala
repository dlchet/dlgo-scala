package scoring

import org.scalatest.wordspec._
import main.Point
import main.Player._
import main.Board

class TerritorySpec extends AnyWordSpec {
  "territory class" should {
    "count black stones, black territory, dame, and track dame points" in {
      val board = new Board(5, 5)
      board.placeStone(Black, new Point(1, 1))
      board.placeStone(White, new Point(2, 5))
      val t = new Territory(board)
      assert(t.numBlackStones == 1)
      assert(t.numBlackTerritory == 0)
      assert(t.numDame == 23)
      assert(t.damePoints.size == 23)
    }
  }
}
