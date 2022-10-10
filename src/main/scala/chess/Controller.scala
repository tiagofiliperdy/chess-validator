package chess

import cats.MonadError
import cats.data.Validated
import cats.implicits._
import chess.app.Configuration.{AppOp, Error, readEnv}
import chess.app.Syntax._
import chess.board.Board
import chess.positions.Position

//TODO: Game is not 100% working, because of:
//  - There is no logic to evaluate check-mate
//  - Since there is no check-mate can't say a game is over, so code runs forever.
//  To change the above logic for check-mate needs to be implemented, at the end of each round
//  validate it, if it's check-mate then break the loop.
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
      state <- env.boardService.currentState.toAppOp
      _ <- env.console.printLine(state.show).toAppOp
    } yield ()

  def readPlay: AppOp[UserInputPlay] =
    for {
      env <- readEnv
      line <- env.console.readLine(">>> Player 1 play:").toAppOp
      positions <- Position(line).toAppOp
    } yield UserInputPlay(positions._1, positions._2)

  def validatePlay(
    input: UserInputPlay,
    board: Board
  ): AppOp[Move] =
    for {
      move <- Move(board, input.from, input.to).toAppOp
      _ <- move.piece.isValidMoveV2(move, board).toAppOp
      kingInCheck <- Board.isKingInCheck(board, move).toAppOp
      _ <- if (kingInCheck) Validated.invalidNec("Move is invalid, you can't leave your king in check!").toAppOp
      else ().pure[AppOp]
    } yield move

  def updateBoard(move: Move): AppOp[Unit] =
    for {
      env <- readEnv
      newBoard <- env.boardService.updateBoard(move).toAppOp
      _ <- env.console.printLine(newBoard.show).toAppOp
    } yield ()

  def game(): AppOp[Boolean] =
    for {
      env <- readEnv
      userInput <- readPlay
      boardState <- env.boardService.currentState.toAppOp
      move <- validatePlay(userInput, boardState.board)
      _ <- updateBoard(move)
      weHaveWinner <- isGameFinished()
    } yield weHaveWinner

  // simulates the check-mate logic
  def isGameFinished(): AppOp[Boolean] = Validated.valid(false).toAppOp
}
