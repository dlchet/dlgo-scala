package main

import Player._

object Utils {
  private val cols = ('A' to 'T').filter(_ != 'I').mkString
  private def stoneToChar(p: Option[Player]) = p match {
    case Some(value) => if (value == Black) " x " else " o "
    case None        => " . "
  }
  def printMove(player: Player, move: Move): Unit = {
    val moveStr = move match {
      case Pass        => "passes"
      case Resign      => "resigns"
      case Play(point) => s"${cols(point.col - 1)}${point.row}"
    }
    println(s"$player $moveStr")
  }
  def printBoard(board: Board): Unit = {
    (board.numRows to 1 by -1) foreach { (r) =>
      val bump = if (r <= 9) " " else ""
      val line =
        (1 to board.numCols).map((c) =>
          stoneToChar(board.get(new Point(r, c)))
        )
      println(s"$bump$r ${line.mkString}")
    }
    println(s"    ${cols.slice(0, board.numCols).mkString("  ")}")
  }
  def pointFromCoords(coords: String): Point = {
    assert(coords.length >= 2)
    assert(cols.contains(coords(0)))
    val row = coords.drop(1).toInt
    assert((1 to 19).contains(row))
    new Point(row, cols.indexOf(coords(0)) + 1)
  }
}
