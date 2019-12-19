package userinput

import chess.Output

trait MockOutput extends Output {
  var messages: Seq[String] = Seq()

  override def println(s: String): Unit = messages = messages :+ s
}
