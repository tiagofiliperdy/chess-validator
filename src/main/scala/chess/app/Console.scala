package chess.app

import cats.implicits._
import cats.effect.IO
import chess.app.Configuration.Error
import chess.app.Console.{Info, Level, Error => ConsoleError}

import scala.io.{AnsiColor, StdIn}

trait Console {
  val console: Service

  trait Service {
    def readLine(msg: String): IO[String]

    def printLine(line: String, level: Level = Info): IO[Unit]

    def printLines(lines: Error, level: Level = ConsoleError): IO[Unit]
  }

}

trait LiveConsole extends Console {
  override val console: Service = new Service {
    override def readLine(msg: String): IO[String] =
      IO(StdIn.readLine(msg))

    override def printLine(line: String, level: Level): IO[Unit] =
      IO(println(level.getColor + line))

    override def printLines(lines: Error, level: Level): IO[Unit] =
      IO(println(level.getColor + lines.mkString_("\n")))
  }
}

object Console {
  sealed trait Level {
    def getColor: String
  }

  case object Info extends Level {
    override def getColor: String = AnsiColor.WHITE
  }

  case object Error extends Level {
    override def getColor: String = AnsiColor.RED
  }

  case object Success extends Level {
    override def getColor: String = AnsiColor.GREEN
  }
}
