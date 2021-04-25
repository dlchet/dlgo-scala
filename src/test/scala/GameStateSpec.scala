package main

import org.scalatest.wordspec._
import Player._
import NewGame._
import Utils._

class GameStateSpec extends AnyWordSpec {
  "a go game state" should {
    val board = new Board(3, 3)
    val gs    = new GameState(board, Black, None, None)
    "be able to apply a pass move" in {
      assert(
        gs.applyMove(Pass).board == new Board(3, 3)
      )
    }
    "return a pass move that has been applied" in {
      assert(gs.applyMove(Pass).lastMove == Some(Pass))
    }
    "retain game state as previous" in {
      assert(gs.applyMove(Pass).previousState == Some(gs))
    }
    "switch next player after applying a move" in {
      assert(gs.applyMove(Pass).nextPlayer == White)
    }
    "apply a stone placing move" in {
      val myBoard = new Board(3, 3)
      myBoard.placeStone(
        Black,
        new Point(1, 1)
      )
      val newGs = gs.applyMove(new Play(new Point(1, 1)))
      assert(newGs.board == myBoard)
    }
    "not manipulate the input board" in {
      gs.applyMove(new Play(new Point(1, 2)))
      assert(board.get(new Point(1, 2)) == None)
    }
    "be initializable via a call to newGame" in {
      assert(newGame(3).board == board)
    }
    "know when a game is over" in {
      assert(gs.applyMove(Resign).isOver())
      assert(!gs.isOver())
      assert(gs.applyMove(Pass).applyMove(Pass).isOver())
    }
    "know if a move is a self-capture" in {
      val initGs = newGame(5)
      val newGs = initGs
        .applyMove(Play(new Point(1, 1)))
        .applyMove(Play(new Point(1, 3)))
        .applyMove(Play(new Point(5, 5)))
        .applyMove(Play(new Point(2, 3)))
        .applyMove(Play(new Point(4, 5)))
        .applyMove(Play(new Point(2, 2)))
        .applyMove(Play(new Point(3, 5)))
        .applyMove(Play(new Point(2, 1)))
      assert(newGs.isMoveSelfCapture(Black, Play(new Point(1, 2))))
      assert(!initGs.isMoveSelfCapture(Black, Play(new Point(1, 2))))
    }
    "know if a move violates ko" in {
      val initGs = newGame(5)
      assert(!initGs.doesMoveViolateKo(Black, Play(new Point(1, 1))))
      assert(
        initGs
          .applyMove(Play(new Point(1, 1)))
          .applyMove(Play(new Point(5, 2)))
          .applyMove(Play(new Point(3, 1)))
          .applyMove(Play(new Point(4, 1)))
          .applyMove(Play(new Point(2, 2)))
          .applyMove(Play(new Point(3, 2)))
          .applyMove(Play(new Point(3, 3)))
          .applyMove(Play(new Point(4, 3)))
          .applyMove(Play(new Point(4, 2)))
          .doesMoveViolateKo(White, Play(new Point(3, 2)))
      )
    }
  }
}
