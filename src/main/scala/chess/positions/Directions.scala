package chess.positions

import chess.{File, Rank}

object Directions {

  sealed trait Direction {
    val shift: (File, Rank)
  }

  case object N extends Direction { val shift = (0, 1) }
  case object S extends Direction { val shift = (0, -1) }
  case object E extends Direction { val shift = (1, 0) }
  case object W extends Direction { val shift = (-1, 0) }
  case object NE extends Direction { val shift = (1, 1) }
  case object NW extends Direction { val shift = (-1, 1) }
  case object SE extends Direction { val shift = (1, -1) }
  case object SW extends Direction { val shift = (-1, -1) }

  val all: Set[Direction] = Set(N, S, E, W, NE, NW, SE, SW)

}
