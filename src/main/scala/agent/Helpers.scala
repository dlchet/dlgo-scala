package agent

import main.Player._
import main.Board
import main.Point

object Helpers {
  def isPointAnEye(board: Board, point: Point, color: Player): Boolean = {
    if (board.get(point) != None) return false
    point.neighbors foreach { n =>
      if (board.isOnGrid(n)) board.get(n) match {
        case Some(value) => if (value != color) return false
        case _           =>
      }
    }
    val (offBoardCorners, friendlyCorners) = List(
      new Point(point.row - 1, point.col - 1),
      new Point(point.row - 1, point.col + 1),
      new Point(point.row + 1, point.col - 1),
      new Point(point.row + 1, point.col - 1)
    ).foldLeft((0, 0)) { case ((oc, fc), c) =>
      if (board.isOnGrid(c)) board.get(c) match {
        case Some(value) => (oc, fc + 1)
        case None        => (oc, fc)
      }
      else (oc + 1, fc)
    }
    if (offBoardCorners > 0) return offBoardCorners + friendlyCorners == 4
    else return friendlyCorners >= 3
    true
  }
}
