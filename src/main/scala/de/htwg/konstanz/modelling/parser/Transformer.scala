package de.htwg.konstanz.modelling.parser

import de.htwg.konstanz.modelling.model.Record

object Transformer {
  def apply(parseResult: Either[String, Record]): Either[String, Record] =
    for {
      parsed <- parseResult
      fields <- checkFields(parsed)
      dataTypes <- checkDataType(fields)
    } yield {
      dataTypes
    }

  def checkFields(record: Record): Either[String, Record] =
    if (record.fields.isEmpty)
      Left("A Record without fields does not make sense!")
    else if (!record.fields.exists(a => "variety".equals(a.name)))
      Left("A Record must have a variety field")
    else if (!record.fields.exists(a => "type".equals(a.name)))
      Left("A Record must have a type field")
    else
      Right(record)

  def checkDataType(record: Record): Either[String, Record] = {

    val dataTypes = record.fields
      .map(field => field.dataType)

    if (dataTypes.exists(dt => dt.enumeration.isDefined && dt.fixedValue.isDefined))
      Left("Either a fixed value can be defined or a enumeration, but not both at the same time")
    else if (dataTypes.exists(dt => dt.floatDigitsBeforeDecimal.isDefined != dt.floatDigitsAfterDecimal.isDefined))
      Left("Either both values of decimal digits must be defined or none")
    else
      Right(record)
  }
}
