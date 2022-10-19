package chess.app

import chess.common.{Black, Color, White}

sealed abstract class Player(
  val color: Color,
  val denomination: String
) { def getOpponent: Player }

object Player {
  case object P1 extends Player(White, "Player 1") {
    override def getOpponent: Player = P2
  }
  case object P2 extends Player(Black, "Player 2") {
    override def getOpponent: Player = P1
  }
}
