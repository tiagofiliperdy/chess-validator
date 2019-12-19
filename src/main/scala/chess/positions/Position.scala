package chess.positions

import chess.{File, Rank}

case class Position(
  file: File,
  rank: Rank
) {

  def diff(pos: Position): (File, Rank) =
    (pos.file - file, pos.rank - rank)
}
