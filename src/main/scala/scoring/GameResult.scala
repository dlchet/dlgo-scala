package scoring

import main.GameState
import main.Player._

class GameResult(
    _gameState: GameState,
    _komi: Int = 7,
    _tiebreak: Boolean = true
) {
  val _territory  = new Territory(_gameState.board)
  private val _tb = if (_tiebreak) 0.5 else 0.0
  val b =
    _territory.numBlackStones + _territory.numBlackTerritory
  val w =
    _territory.numWhiteStones + _territory.numWhiteTerritory + _komi + _tb
  def winner(): Player = if (b > w) Black else White
  def winningMargin()  = (b - w).abs
  val blackTerrString =
    s"bs: ${_territory.numBlackStones}; bt: ${_territory.numBlackTerritory}"
  val whiteTerrString =
    s"ws: ${_territory.numWhiteStones}; wt: ${_territory.numWhiteTerritory}"
  val dameString = s"d: ${_territory.numDame}; dp: ${_territory.damePoints}"
  val debugString =
    List(blackTerrString, whiteTerrString, dameString) mkString ("\n\t")
  override def toString(): String = winner() match {
    case Black => s"B+$winningMargin ($b > $w)\n\t$debugString"
    case _     => s"W+$winningMargin ($w > $b)\n\t$debugString"
  }
}
