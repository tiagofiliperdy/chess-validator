package chess.app

import cats.Eq
import chess.common.Color.{Black, White}
import chess.common.Color

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

  implicit def eqPlayer(implicit eqColor: Eq[Color]): Eq[Player] = Eq.by(_.color)
}
