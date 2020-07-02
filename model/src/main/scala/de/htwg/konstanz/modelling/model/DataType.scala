package de.htwg.konstanz.modelling.model

case class DataType(
                     name: String,
                     position: Int,
                     length: Int,
                     fixedValue: Option[String],
                     floatDigitsBeforeDecimal: Option[Int],
                     floatDigitsAfterDecimal: Option[Int],
                     enumeration: Option[Enumeration]
                   )
