package main

import Player._
import Utils._
import scala.annotation.tailrec

class GameState(
    val board: Board,
    val nextPlayer: Player,
    val previousState: Option[GameState],
    val lastMove: Option[Move]
) {
  val previousStates: Set[(Player, Long)] = previousState match {
    case Some(value) =>
      value.previousStates + ((value.nextPlayer, value.board.zobristHash))
    case None => Set[(Player, Long)]()
  }
  def applyMove(move: Move): GameState = {
    var newBoard = board
    move match {
      case Play(point) => {
        newBoard = board.copy()
        newBoard.placeStone(nextPlayer, point)
      }
      case _ =>
    }
    new GameState(newBoard, nextPlayer.other, Some(this), Some(move))
  }
  def isOver(): Boolean = {
    lastMove match {
      case Some(Resign) => true
      case Some(Pass) =>
        previousState match {
          case None => return false
          case Some(value) =>
            value.lastMove match {
              case Some(Pass) => return true
              case _          => return false
            }
        }
      case _ => false
    }
  }
  def isMoveSelfCapture(player: Player, move: Move): Boolean = {
    move match {
      case Pass   => false
      case Resign => false
      case Play(point) => {
        val nextBoard = board.copy()
        nextBoard.placeStone(player, point)
        val newString = nextBoard.getGoString(point)
        assert(newString != None)
        newString match {
          case Some(value) => return value.numLiberties() == 0
          case None        => false // unreachable
        }
      }
    }
  }
  def doesMoveViolateKo(player: Player, move: Move): Boolean = {
    move match {
      case Play(point) => {
        val nextBoard = board.copy()
        nextBoard.placeStone(player, point)
        val nextSituation = (player.other, nextBoard.zobristHash)
        return previousStates.contains(nextSituation)
      }
      case _ => return false
    }
  }
  def isValidMove(move: Move): Boolean = {
    if (isOver) return false
    move match {
      case Play(point) =>
        return board.get(point) == None && !isMoveSelfCapture(
          nextPlayer,
          move
        ) && !doesMoveViolateKo(nextPlayer, move)
      case _ => return true
    }
  }
}

object NewGame {
  def newGame(dim: Int): GameState = {
    assert(dim > 0)
    new GameState(new Board(dim, dim), Black, None, None)
  }
}
