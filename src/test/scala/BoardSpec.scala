package main

import org.scalatest.wordspec._
import Player._
import Utils._

class BoardSpec extends AnyWordSpec {
  "a go board" should {
    val point  = new Point(1, 1)
    val oneTwo = new Point(1, 2)
    "have a count of rows and columns" in {
      val board = new Board(3, 3)
      assert(board.numRows == 3)
      assert(board.numCols == 3)
    }
    "determine if a point is on the grid" in {
      val board = new Board(3, 3)
      assert(board.isOnGrid(point))
    }
    "determine that a point off the grid is off the grid" in {
      val board = new Board(3, 3)
      assert(!board.isOnGrid(new Point(1, 4)))
    }
    "be able to place a stone and get back a go string" in {
      val board = new Board(3, 3)
      board.placeStone(Black, point)
      assert(board.get(point) == Some(Black))
      assert(board.get(new Point(2, 2)) == None)
    }
    "be able to get a multi-stone go string" in {
      val board = new Board(3, 3)
      board.placeStone(Black, point)
      board.placeStone(Black, oneTwo)
      val resultString = board.getGoString(point)
      resultString match {
        case Some(value) => assert(value.stones == Set(point, oneTwo))
        case None        => assert(false)
      }
    }
    "not combine go strings of different colors" in {
      val board = new Board(3, 3)
      board.placeStone(Black, point)
      board.placeStone(White, oneTwo)
      val resultString = board.getGoString(point)
      resultString match {
        case Some(value) => assert(value.stones == Set(point))
        case None        => assert(false)
      }
    }
    "error when placing a stone off the grid" in {
      val board = new Board(3, 3)
      board.placeStone(Black, point)
      intercept[AssertionError] {
        board.placeStone(White, new Point(4, 1))
      }
    }
    "error when placing a stone on an already filled point" in {
      val board = new Board(3, 3)
      board.placeStone(Black, point)
      intercept[AssertionError] {
        board.placeStone(Black, point)
      }
    }
    "count an unoccupied point as a liberty" in {
      val board = new Board(3, 3)
      board.placeStone(Black, point)
      assert(
        board.getGoString(point) == Some(
          new GoString(Black, Set(point), Set(oneTwo, new Point(2, 1)))
        )
      )
    }
    "not create a liberty off the grid" in {
      val board = new Board(3, 3)
      board.placeStone(Black, new Point(3, 3))
      assert(
        board.getGoString(new Point(3, 3)) == Some(
          new GoString(
            Black,
            Set(new Point(3, 3)),
            Set(new Point(3, 2), new Point(2, 3))
          )
        )
      )
    }
    "not create a liberty from a point which is occupied by the other color" in {
      val board = new Board(3, 3)
      board.placeStone(Black, point)
      board.placeStone(White, oneTwo)
      val liberties = board.getGoString(oneTwo)
      liberties match {
        case None => assert(false)
        case Some(value) =>
          assert(value.liberties == Set(new Point(2, 2), new Point(1, 3)))
      }
    }
    "not keep a liberty from a point which has been subsequently occupied by the other color" in {
      val board = new Board(3, 3)
      board.placeStone(Black, point)
      board.placeStone(White, oneTwo)
      val liberties = board.getGoString(point)
      liberties match {
        case None        => assert(false)
        case Some(value) => assert(value.liberties == Set(new Point(2, 1)))
      }
    }
    "remove a string that has been captured" in {
      val board = new Board(3, 3)
      board.placeStone(Black, point)
      board.placeStone(White, oneTwo)
      board.placeStone(White, new Point(2, 1))
      assert(board.getGoString(point) == None)
    }
    "add a liberty for a string that has gained an empty neighbor" in {
      val board = new Board(3, 3)
      board.placeStone(Black, point)
      board.placeStone(White, oneTwo)
      board.placeStone(White, new Point(2, 1))
      board.getGoString(oneTwo) match {
        case None        => assert(false)
        case Some(value) => assert(value.liberties.contains(point))
      }
    }
    "not be equal if rows differ" in {
      val board      = new Board(3, 3)
      val largeBoard = new Board(4, 4)
      assert(board != largeBoard)
    }
    "be able to be copied" in {
      val board = new Board(3, 3)
      board.placeStone(Black, point)
      val newBoard = board.copy()
      newBoard.placeStone(White, oneTwo)
      assert(board.get(oneTwo) == None)
      assert(newBoard.get(point) == Some(Black))
      assert(newBoard.get(oneTwo) == Some(White))
    }
    "not affect original grid from copy" in {
      val board = new Board(3, 3)
      board.placeStone(Black, point)
      val newBoard = board.copy()
      newBoard.placeStone(White, oneTwo)
      board.getGoString(point) match {
        case Some(value) => assert(value.liberties.contains(oneTwo))
        case None        => assert(false)
      }
    }
  }
}
