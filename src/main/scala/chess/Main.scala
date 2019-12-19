package chess

object Main extends App {
  val fileName = "src/main/resources/data/invalid-pawn-moves.txt"

  // TODO: implement and test check validation
  // TODO: folder null log4j investigation
  // TODO: Ensure that you follow best-practices, design principles like SOLID and DRY.
  // TODO: push to personal git
  // TODO: try it out on another pc
  new Controller(fileName).init()

}
