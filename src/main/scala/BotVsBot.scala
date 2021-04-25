package main

import NewGame._
import Player._
import Utils._
import scala.collection.immutable.HashMap
import agent.RandomBot

object BotVsBot extends App {
  val boardSize = 9
  var game      = newGame(boardSize)
  val bots      = HashMap((Black, new RandomBot), (White, new RandomBot))
  while (!game.isOver()) {
    Thread.sleep(300)
    print(27.toChar +: "[2J")
    printBoard(game.board)
    val botMove = bots(game.nextPlayer).selectMove(game)
    printMove(game.nextPlayer, botMove)
    game = game.applyMove(botMove)
  }
}
