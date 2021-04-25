package agent

import main.{GameState, Move, Play, Pass, Point}
import Helpers._
import scala.util.Random

class RandomBot extends Agent {
  def selectMove(gameState: GameState): Move = {
    val candidates = (1 to gameState.board.numRows)
      .flatMap((r) =>
        (1 to gameState.board.numCols).map((c) => new Point(r, c))
      )
      .filter((p) =>
        gameState.isValidMove(Play(p)) && !isPointAnEye(
          gameState.board,
          p,
          gameState.nextPlayer
        )
      )
    candidates match {
      case c if c.size == 0 => Pass
      case c                => Play(c(Random.nextInt(c.length)))
    }
  }
}
