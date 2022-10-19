package chess

import cats.MonadError
import cats.data.Validated
import cats.implicits._
import chess.app.Configuration.{AppOp, Error, readEnv}
import chess.app.Console.{Error => ConsoleError}
import chess.app.Move
import chess.app.Syntax._
import chess.board.Board
import chess.positions.Position

//  TODO: Game is not 100% working, because of:
//    - There is no logic to evaluate check-mate, so code runs forever.

object Controller {
  case class UserInputPlay(
    from: Position,
    to: Position
  )

  val ME = MonadError[AppOp, Error]

  def run(): AppOp[Unit] = {
    def recovery: Error => AppOp[Boolean] = { error: Error =>
      for {
        env <- readEnv
        _ <- env.console.printLines(error).toAppOp
      } yield false
    }

    setupGame >> ME.iterateUntil(ME.handleErrorWith(game())(recovery))(identity).void
  }

  def setupGame: AppOp[Unit] =
    for {
      env <- readEnv
      _ <- env.console.printLine(">>> Game Begins!").toAppOp
      state <- env.gameService.currentState.toAppOp
      _ <- env.console.printLine(state.show).toAppOp
    } yield ()

  def readPlay: AppOp[UserInputPlay] =
    for {
      env <- readEnv
      state <- env.gameService.currentState.toAppOp
      line <- env.console.readLine(s">>> ${state.player.denomination} play:").toAppOp
      positions <- Position(line).toAppOp
    } yield UserInputPlay(positions._1, positions._2)

  def validatePlay(input: UserInputPlay): AppOp[Move] =
    for {
      env <- readEnv
      state <- env.gameService.currentState.toAppOp
      move <- Move(state.board, input.from, input.to, state.player).toAppOp
      _ <- move.piece.isValidMove(move, state.board).toAppOp
      _ <- isKingInCheck
    } yield move

  def isKingInCheck: AppOp[Unit] =
    for {
      env <- readEnv
      state <- env.gameService.currentState.toAppOp
      kingInCheck <- Board.isKingInCheck(state.board, state.player).toAppOp
      _ <- if (kingInCheck) Validated.invalidNec("Move is invalid, you can't leave your king in check!").toAppOp
      else ().pure[AppOp]
    } yield ()

  def informIsKingInCheck(): AppOp[Unit] =
    for {
      env <- readEnv
      state <- env.gameService.currentState.toAppOp
      kingInCheck <- Board.isKingInCheck(state.board, state.player).toAppOp
      _ <- if (kingInCheck) env.console.printLine("Your King is in check, play wise!", ConsoleError).toAppOp
      else ().pure[AppOp]
    } yield ()

  def updateBoard(move: Move): AppOp[Unit] =
    for {
      env <- readEnv
      newBoard <- env.gameService.updateBoard(move).toAppOp
      _ <- env.console.printLine(newBoard.show).toAppOp
    } yield ()

  def game(): AppOp[Boolean] =
    for {
      _ <- informIsKingInCheck()
      userInput <- readPlay
      move <- validatePlay(userInput)
      _ <- updateBoard(move)
      weHaveWinner <- isGameFinished
    } yield weHaveWinner

  def isGameFinished: AppOp[Boolean] = {
    for {
      env <- readEnv
      _ <- env.gameService.switchTurns().toAppOp
      // simulates the check-mate logic
      res <- Validated.valid(false).toAppOp
    } yield res
  }
}
