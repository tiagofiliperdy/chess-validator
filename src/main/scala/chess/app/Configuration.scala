package chess.app

import cats.data._
import cats.effect.IO
import chess.board.GameService.GameState
import chess.board.{GameService, LiveGameService}

object Configuration {
  type Error = NonEmptyChain[String]
  type IsValid[A] = Validated[Error, A] // allows to group multiple validation errors
  type ErrorOr[A] = EitherT[IO, Error, A] // allows to differentiate between different types of error levels (logic or external)
  type St[A] = StateT[ErrorOr, GameState, A] // Makes it easy to track state of the board

  type Environment = GameService with Console // Environment is nothing less than injecting dependencies
  val liveEnv: Environment = new LiveGameService with LiveConsole // Environment which the app runs normally
  type AppOp[A] = ReaderT[St, Environment, A] // Generic type of the whole App

  def readEnv: AppOp[Environment] = ReaderT.ask[St, Environment] // Allows to fetch the environment at any time
}
