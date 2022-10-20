package chess.app

import Syntax._
import cats.data.Validated
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import chess.FpFinalSpec
import chess.app.Configuration.{IsValid, liveEnv}
import chess.service.GameService.{GameOp, GameState}

class SyntaxSpec extends FpFinalSpec {

  test("IO toAppOp") {
    forAll { io: IO[Int] =>
      val appState = GameState.beginning
      assert(
        io.toAppOp.run(liveEnv).run(appState).value.unsafeRunSync() eqv Right(
          (appState, io.unsafeRunSync())
        )
      )
    }
  }

  test("IsValid[A] toAppOp") {
    forAll { isValid: IsValid[Int] =>
      val appState = GameState.beginning
      val result = isValid.toAppOp.run(liveEnv).run(appState).value.unsafeRunSync()
      isValid match {
        case Validated.Valid(a) => assert(result eqv Right((appState, a)))
        case Validated.Invalid(_) => assert(result.isLeft)
      }
    }
  }

  test("GameOp toAppOp") {
    forAll { gameOp: GameOp[String] =>
      val appState = GameState.beginning
      val (gs, str) = gameOp.run(appState).value
      val result = gameOp.toAppOp.run(liveEnv).run(appState).value.unsafeRunSync()

      assert(result eqv Right((gs, str)))
    }
  }

}
