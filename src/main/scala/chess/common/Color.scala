package chess.common

import cats.Eq

sealed trait Color

object Color {
  case object White extends Color
  case object Black extends Color

  implicit val eqColor: Eq[Color] =
    Eq.instance {
      case (White, White) => true
      case (Black, Black) => true
      case _              => false
    }
}
