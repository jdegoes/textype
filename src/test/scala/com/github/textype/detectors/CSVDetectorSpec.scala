package com.github.textype.detectors

import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.scalacheck._
import org.scalacheck.Arbitrary._

import com.github.textype._

class CSVDetectorSpec extends Specification with ScalaCheck {
  import CSVSamples._

  case class CSVDims(rows: Int, cols: Int)
  case class CSVParameters(sep: String, quote: String, escQuote: String, newline: String) {
    def isClean(v: String) = !v.contains(sep) & !v.contains(quote) & !v.contains(escQuote) & !v.contains(newline)
  }
  case class CSVField(value: String)
  case class CSVRecord(value: String)  
  case class CSVFile(dims: CSVDims, params: CSVParameters, value: String)

  implicit def arbitraryCSVDims = Arbitrary {
    for {
      rows <- Gen.choose(5, 10)
      cols <- Gen.choose(5, 10)
    } yield CSVDims(rows, cols)
  }

  implicit def arbitraryCSVParameters = Arbitrary { 
    for { 
      sep      <- Gen.oneOf(",", "|", "\t", ":")
      quote    <- Gen.oneOf("\"", "\'")
      escQuote <- Gen.oneOf("\"\"", "''")
      newline  <- Gen.oneOf("\r\n", "\n")
    } yield CSVParameters(sep, quote, escQuote, newline)
  }

  def genCSVString(params: CSVParameters): Gen[String] = {
    val chars = Set("1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", " ", ".", "(", ")", "\"", "'", "`")

    val chars2 = (chars - params.newline - params.quote - params.escQuote - params.sep).toSeq

    for {
      len <- Gen.choose(0, 10)
      chars <- Gen.listOfN(len, Gen.oneOf(chars2))
    } yield chars.mkString("")
  }

  def genCSVField(params: CSVParameters): Gen[CSVField] = {
    val genString = genCSVString(params)

    for {
      left <- genString
      right <- genString
      quoted <- arbitrary[Boolean]
      middle <- genString
      escQuote <- arbitrary[Boolean]
      embedNewline <- arbitrary[Boolean]
    } yield {
      CSVField(if (quoted) {
        params.quote + left + (
          (if (escQuote) params.escQuote else "") + (if (embedNewline) params.newline else "")
        ) + middle + right + params.quote
      } else left + right)
    }
  }

  def genCSVRecord(dims: CSVDims, params: CSVParameters): Gen[CSVRecord] = {
    for {
      fields <- Gen.listOfN(dims.cols, genCSVField(params))
    } yield CSVRecord(fields.map(_.value).mkString(params.sep))
  }

  implicit def arbitraryCSVFile: Arbitrary[CSVFile] = Arbitrary {
    for {
      params <- arbitrary[CSVParameters]
      dims <- arbitrary[CSVDims]
      rows <- Gen.listOfN(dims.rows, genCSVRecord(dims, params))
    } yield CSVFile(dims, params, rows.map(_.value).mkString(params.newline))
  }

  "CSVDetector" should {
    "detect standard encoding" in {
      val format = CSVDetector(StandardEncoding1).head

      format.sep mustEqual ","
      format.quote mustEqual "\""
      format.newline mustEqual "\n"
      format.headers mustEqual Seq("transaction_unique_identifier", "seller_company_name", "customer_company_name", "customer_duns_number", 
        "tariff_reference", "contract_service_agreement", "trans_id", "transaction_begin_date", "transaction_end_date", "time_zone", 
        "point_of_delivery_control_area", "specific location", "class_name", "term_name", "increment_name", "increment_peaking_name", "product_name", 
        "transaction_quantity", "price", "units", "total_transmission_charge", "transaction_charge")
    }

    "parse all csv files" in {
      check { (file: CSVFile) => 
        val format = CSVDetector(file.value).head

        format.columns must beSome(file.dims.cols)
        format.newline mustEqual file.params.newline
        format.sep mustEqual file.params.sep
        format.quote mustEqual file.params.quote
      }
    }
  }

  "CSVDetector.parser" should {
    "preserve field order" in {
      val example = """name, age, gender
John Doe, 22, M
Mary Jane, 23, F"""

      val format = CSVDetector(example).head

      format.parser.trim.parseAll(example) mustEqual
        Right(Seq(
          Seq("name", "age", "gender"),
          Seq("John Doe", "22", "M"),
          Seq("Mary Jane", "23", "F")
        ))
    }

    "kick out quotes" in {
      val example = """name, age, gender
"Doe, John", 22, M
"Jane, Mary", 23, F"""

      val format = CSVDetector(example).head

      format.parser.trim.parseAll(example) mustEqual
        Right(Seq(
          Seq("name", "age", "gender"),
          Seq("Doe, John", "22", "M"),
          Seq("Jane, Mary", "23", "F")
        ))
    }
  }
}