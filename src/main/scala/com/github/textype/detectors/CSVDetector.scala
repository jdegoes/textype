package com.github.textype.detectors

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import scala.collection.mutable.StringBuilder
import util.control.Breaks._

import com.github.textype._

/**
 * Dumb detector which tries to parse common CSV file formats. 
 */
object CSVDetector extends Detector {
  private val Seps      = Array(",", "|", "\t", ":", " ")
  private val Quotes    = Array("\"", "\'")
  private val EscQuotes = Array("\"\"", "''")
  private val Newlines  = Array("\r\n", "\n")
  // TODO: private val EscChar   = Array("\\")

  private case class ParseState(inQuote: Boolean, startField: String, parsed: List[TextRecord]) {
    def finish = (if (parsed.head.length > 0 || startField.length > 0) ((parsed.head :+ startField) :: parsed.tail) else parsed.tail).reverse

    def score: Int = 0
  }

  private object ParseState {
    val Zero: ParseState = ParseState(false, "", List(Vector.empty[String]))
  }

  private case class CSVRecordParser(Sep: String, Quote: String, EscQuote: String, Newline: String, state: ParseState) 
      extends TextRecordParser { self =>

    def parse(text: Option[String]): (Seq[TextRecord], Either[String, TextRecordParser]) = {
      text match {
        case None =>
          if (state.inQuote) {
            (state.finish, Left("Expected end of quote but found EOF"))
          } else (state.finish, Right(self))

        case Some(text) =>
          var consumed = 0

          var startField = new StringBuilder(state.startField)
          var inQuote    = state.inQuote
          var parsed     = state.parsed

          while (consumed < text.length) {     
            val cur = text.substring(consumed) 

            if (cur.startsWith(Sep)) {
              if (!inQuote) {
                parsed = (parsed.head :+ startField.toString) :: parsed.tail
                
                startField.clear()
              } else startField.append(Sep)

              consumed += Sep.length
            } else if (cur.startsWith(EscQuote)) {
              startField.append(Quote)

              consumed += EscQuote.length
            } else if (cur.startsWith(Quote)) {
              inQuote = !inQuote

              consumed += Quote.length
            } else if (cur.startsWith(Newline)) {
              if (!inQuote) {
                // Add a new empty row with the final field:
                parsed = Vector.empty[String] :: (parsed.head :+ startField.toString) :: parsed.tail

                startField.clear()
              } else startField.append(Newline)

              consumed += Newline.length
            } else {
              startField.append(text(consumed))

              consumed += 1
            }
          }

          val finished = parsed.drop(1)
          val remaining = parsed.take(1)

          (finished.reverse, Right(copy(state = ParseState(inQuote, startField.toString, remaining))))
      }
    }
  }

  def apply(value: String): Stream[CSV] = {
    def score(parsed: Seq[TextRecord]): Score = {
      val lengths = parsed.map(_.length)

      val stats = if (lengths.length == 0) {
        None
      } else {
        val min = lengths.min

        val max = lengths.max

        val mean = lengths.sum / lengths.length.toDouble

        val stdDev = (lengths.map { v =>
          val d = v - mean
          d * d
        }).sum / lengths.length.toDouble

        Some(ScoreColumnStats(min, max, mean, stdDev))
      }

      val values = lengths.sum

      // CSV will always parse 100% of everything:
      Score(1.0, values, stats)
    }

    val parsers = for {
      sep <- Seps
      quote <- Quotes
      escQuote <- EscQuotes
      newline <- Newlines
    } yield CSVRecordParser(sep, quote, escQuote, newline, ParseState.Zero)

    val scored = parsers.flatMap { parser =>
      parser.parse(Some(value)) match {
        case (parsed, _) if (parsed.length > 0) => 
          // TODO: Detect if headers are present
          val headers = parsed(0)

          (score(parsed), headers, parser) :: Nil

        case _ => Nil
      }
    }

    scored.toSeq.sortBy(_._1.value).map({
      case (score, headers, parser) =>
        CSV(score, parser.Sep, parser.Quote, parser.EscQuote, parser.Newline, headers, parser)
    }).toStream 
  }
}