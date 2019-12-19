package chess

sealed trait InputFileError {
  val errorMsg: String
}

final case class EmptyFileError(filePath: String) extends InputFileError {
  val errorMsg = s"File with path '$filePath' has reached EOF."
}

final case class FileNotExistError(filePath: String) extends InputFileError {
  val errorMsg = s"File with path '$filePath' doesn't exist."
}

final case class EOFError(filePath: String) extends InputFileError {
  val errorMsg = s"File with path '$filePath' has reached an empty line."
}
