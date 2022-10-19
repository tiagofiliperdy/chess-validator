package chess.app

import chess.common.{Black, Color, White}

sealed abstract class Player(
  val color: Color,
  val denomination: String,
  val opponent: Player
)

object Player {
  case object P1 extends Player(White, "Player 1", P2)
  case object P2 extends Player(Black, "Player 2", P1)
}
