package chess

import cats.effect.unsafe.implicits.global
import chess.app.Configuration.liveEnv
import chess.service.GameService.GameState

object Main extends App {

  Controller
    .run()
    .run(liveEnv)
    .run(GameState.beginning)
    .value
    .unsafeRunSync()
}
