package com.github.textype

sealed trait TextFileType {
  def headers: TextRecord

  def columns: Option[Int] = score.columnStats.map(_.max)

  def parser: TextRecordParser

  def score: Score
}
case class CSV(score: Score, sep: String, quote: String, escQuote: String, newline: String, headers: TextRecord, parser: TextRecordParser) extends TextFileType

// case class Log(score: Score, headers: Seq[String], columns: Option[Int], parser: TextRecordParser[Seq[String]]) extends TextFileType