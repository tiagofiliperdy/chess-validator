package chess.fakes

import cats.effect.IO
import chess.app.{Configuration, Console}

trait FakeConsole extends Console {
  var linesToRead: List[String]
  var linesWritten: List[String] = List.empty

  override val console: Service = new Service {
    override def readLine(msg: String): IO[String] = IO {
      val head = linesToRead.head
      linesToRead = linesToRead.tail
      head
    }

    override def printLine(line: String, level: Console.Level): IO[Unit] = IO {
      linesWritten = linesWritten :+ line
    }

    override def printLines(lines: Configuration.Error, level: Console.Level): IO[Unit] = IO {
      linesWritten = linesWritten ++ lines.toNonEmptyList.toList
    }
  }

}
