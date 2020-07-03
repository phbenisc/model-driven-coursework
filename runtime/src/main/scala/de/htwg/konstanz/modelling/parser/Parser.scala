package de.htwg.konstanz.modelling.parser

import de.htwg.konstanz.modelling.model.{DataType, EnumValue, Enumeration, Field, Record}

import scala.util.parsing.combinator._

class Parser extends RegexParsers {

  def parseDSL(input: String): Either[String, Record] = parsePartDsl(input, record)

  def parsePartDsl [T](input: String, parser: Parser[T]): Either[String, T] =
    parseAll(parser, input) match {
    case Success(t, _) => Right(t)
    case NoSuccess(msg, next) =>
    val pos = next.pos
    Left(s"[$pos] failed parsing: $msg\n\n${pos.longString})")
  }

  def text: Parser[String] = """[\w]+""".r ^^ (_.toString)
  def integer: Parser[Int] = """(0|[1-9]\d*)""".r ^^ (_.toInt)
  def intTuple: Parser[(Int, Int)] = "(" ~ integer ~ "," ~ integer ~ ")" ^^ { case _ ~ first ~ _ ~ second ~ _ => (first, second) }

  def record: Parser[Record] = "Record" ~ text ~ "{" ~ fields ~ "}" ^^ { case _ ~ name ~ _ ~ f ~ _ => Record(name, f) }
  def fields: Parser[List[Field]] = rep(field)
  def field: Parser[Field] = "Field" ~ opt("required") ~ text ~ ":" ~ dataType ^^ { case _ ~ required ~ name ~ _ ~ data => Field(name, required.isDefined, data) }


  def dataType: Parser[DataType] =
    text ~ opt(intTuple) ~ "pos" ~ intTuple  ~ opt(fixedValue) ~ opt(enumeration) ^^ { case name ~ optFloat ~ _ ~ pos ~ optFixVal ~ optEnum  =>
      DataType(
        name,
        pos._1,
        pos._2,
        optFixVal,
        optFloat.map(_._1),
        optFloat.map(_._2),
        optEnum
      )
    }

  def fixedValue: Parser[String] = "as" ~ "fixedValue " ~ text ^^ { case _ ~ _ ~ value => value}

  def enumValue: Parser[EnumValue] = """[\w\d+\-]+""".r ^^ (value => EnumValue(value, value))
  def enumValues: Parser[List[EnumValue]] = repsep(enumValue, ",") ^^ (values => values)
  def enumeration: Parser[Enumeration] =
    "as" ~ "enum" ~ "{" ~ enumValues ~ "}" ^^ { case _ ~ _ ~ _  ~ ev  ~ _ =>
      Enumeration(ev) }
}
