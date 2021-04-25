package agent

import main.GameState
import main.Move

abstract class Agent {
  def selectMove(gameState: GameState): Move
}
