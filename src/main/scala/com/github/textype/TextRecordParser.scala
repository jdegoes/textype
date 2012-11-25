package com.github.textype

/**
 * A simple stateful parser, factored as a mealy machine. At every step, capable
 * of producing outputs and either a continuation or an error message.
 */
trait TextRecordParser { self =>
  def parse(text: Option[String]): (Seq[TextRecord], Either[String, TextRecordParser])

  def map(f: TextRecord => TextRecord): TextRecordParser = new TextRecordParser {
    def parse(text: Option[String]): (Seq[TextRecord], Either[String, TextRecordParser]) = {
      self.parse(text) match {
        case (parsed, Left(error)) => (parsed.map(f), Left(error))
        case (parsed, Right(next)) => (parsed.map(f), Right(next.map(f)))
      }
    }
  }

  /**
   * Removes leading and trailing white space around field values.
   */
  def trim: TextRecordParser = map(_.map(_.trim))

  /**
   * A convenience function that parses all the text at once, and either 
   * succeeds with the full results, or fails if there was any error during
   * parsing. 
   */
  def parseAll(text: String): Either[String, Seq[TextRecord]] = {
    parse(Some(text)) match {
      case (init, Left(msg)) => Left(msg)
      case (init, Right(next)) => next.parse(None) match {
        case (tail, Left(msg)) => Left(msg)
        case (tail, Right(next)) => Right(init ++ tail)
      }
    }
  }
}