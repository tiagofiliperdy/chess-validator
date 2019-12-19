package chess

object Main extends App {
  val fileName = "src/main/resources/data/moves.txt"

  new Controller(fileName).init()
}
