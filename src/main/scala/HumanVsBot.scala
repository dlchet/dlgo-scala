package main

import Player._
import Utils._
import NewGame._
import agent.RandomBot

object HumanVsBot extends App {
  val boardSize = 9
  var game      = newGame(boardSize)
  val bot       = new RandomBot

  while (!game.isOver()) {
    print(27.toChar +: "[2J")
    printBoard(game.board)
    val move = if (game.nextPlayer == Black) {
      val humanMove = scala.io.StdIn.readLine()
      val point     = pointFromCoords(humanMove)
      Play(point)
    } else bot.selectMove(game)
    printMove(game.nextPlayer, move)
    game = game.applyMove(move)
  }
}
