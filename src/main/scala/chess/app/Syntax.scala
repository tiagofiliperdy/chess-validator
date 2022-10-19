package chess.app

import cats.data.{EitherT, NonEmptyChain, ReaderT, StateT}
import cats.effect.IO
import cats.implicits._
import chess.app.Configuration.{AppOp, ErrorOr, IsValid, St}
import chess.board.GameService.GameOp

object Syntax {

  implicit class IOOps[A](io: IO[A]) {

    def toAppOp: AppOp[A] = {
      val errorOr: ErrorOr[A] = EitherT(io.attempt.map(_.leftMap(e => NonEmptyChain.one(e.getMessage))))
      val state: St[A] = StateT.liftF(errorOr)
      ReaderT.liftF(state)
    }
  }

  implicit class IsValidOps[A](isValid: IsValid[A]) {

    def toAppOp: AppOp[A] = {
      val errorOr: ErrorOr[A] = EitherT(IO(isValid.toEither))
      val st: St[A] = StateT.liftF(errorOr)
      ReaderT.liftF(st)
    }
  }

  /**
    * When a value of this type is run, it should not produce exceptions. Furthermore,
    * the type does not encode any information about errors. So we can assume that once we have the A
    * we can lift it to EitherT via pure.
    *
    * Important part of StateT layer. We have a BoardState and we want to update BoardState with it.
    * Correct way is to use the current board state of the application to run our board state, so whatever
    * modification we do is node on top of what we already had. Once we have our new board state, update the global
    * board state with it so the changes are reflected.
    *
    */
  implicit class BoardOpOps[A](boardOp: GameOp[A]) {

    def toAppOp: AppOp[A] = {
      val st: St[A] = StateT { appState =>
        val (boardState, a) = boardOp.run(appState).value
        (appState.copy(board = boardState.board, history = boardState.history), a).pure[ErrorOr]
      }
      ReaderT.liftF(st)
    }
  }

}
