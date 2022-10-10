package chess

import cats.effect.unsafe.implicits.global
import chess.app.Configuration.liveEnv
import chess.board.BoardService.BoardState

//TODO:
//  - create property based tests
//  - Create switch turns and player validations
object Main extends App {

  Controller
    .run()
    .run(liveEnv)
    .run(BoardState.beginning)
    .value
    .unsafeRunSync()
}
