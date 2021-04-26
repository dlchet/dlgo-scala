package main

import NewGame._
import Player._
import Utils._
import scala.collection.immutable.HashMap
import agent.RandomBot
import scoring.GameResult

object BotVsBot extends App {
  val boardSize: Int = args.length match {
    case 0 => 9
    case _ =>
      if (args(0).toIntOption.isDefined) args(0).toInt match {
        case n if (n > 0 && n < 20) => n
        case _                      => 9
      }
      else 9
  }
  var game = newGame(boardSize)
  val bots = HashMap((Black, new RandomBot), (White, new RandomBot))
  while (!game.isOver()) {
    Thread.sleep(300)
    print(27.toChar +: "[2J")
    printBoard(game.board)
    val botMove = bots(game.nextPlayer).selectMove(game)
    printMove(game.nextPlayer, botMove)
    game = game.applyMove(botMove)
  }
  println(new GameResult(game))
  printBoard(game.board)
}
