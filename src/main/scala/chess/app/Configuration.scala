package chess.app

import cats.data._
import cats.effect.IO
import chess.board.BoardService.BoardState
import chess.board.{BoardService, LiveBoardService}

object Configuration {
  type Error = NonEmptyChain[String]
  type IsValid[A] = Validated[Error, A] // allows to group multiple validation errors
  type ErrorOr[A] = EitherT[IO, Error, A] // allows to differentiate between different types of error levels (logic or external)
  type St[A] = StateT[ErrorOr, BoardState, A] // Makes it easy to track state of the board

  type Environment = BoardService with Console // Environment is nothing less than injecting dependencies
  val liveEnv: Environment = new LiveBoardService with LiveConsole // Environment which the app runs normally
  type AppOp[A] = ReaderT[St, Environment, A] // Generic type of the whole App

  def readEnv: AppOp[Environment] = ReaderT.ask[St, Environment] // Allows to fetch the environment at any time
}
