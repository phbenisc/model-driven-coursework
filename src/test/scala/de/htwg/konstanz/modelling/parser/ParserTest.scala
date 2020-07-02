package de.htwg.konstanz.modelling.parser

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.{Disabled, Test}

class ParserTest {

  val sut = new Parser()

  @Test
  def testEmptyRecord(): Unit = {

    val c0RecordDsl = "Record baseAmount { }"


    val c0Record = sut.parseDSL(c0RecordDsl)


    assertTrue(c0Record.isRight, c0Record.fold(s => s, _ => ""))
    val value = c0Record.toOption.get;

    assertEquals("baseAmount", value.name)
  }

  @Test
  def testRecordWithOneField(): Unit = {

    val c0RecordDsl =
      """Record baseAmount {
        |     Field required variety:        C          pos ( 1,  1) as fixedValue C
        |}""".stripMargin


    val c0Record = sut.parseDSL(c0RecordDsl)


    assertTrue(c0Record.isRight, c0Record.fold(s => s, _ => ""))
    val value = c0Record.toOption.get;

    assertEquals("baseAmount", value.name)

    val firstField = value.fields.head
    assertEquals("variety", firstField.name)
    assertTrue(firstField.required)
    assertEquals("C", firstField.dataType.name)
    assertEquals(1, firstField.dataType.position)
    assertEquals(1, firstField.dataType.length)
    assertTrue(firstField.dataType.fixedValue.isDefined)
    assertEquals("C", firstField.dataType.fixedValue.getOrElse(""))

  }

  @Test
  def testEnumValue(): Unit = {

    val partDsl = """+""".stripMargin


    val bla = sut.parsePartDsl(partDsl, sut.enumValue)

    val value = bla.toOption.get;

    assertNotNull(value)
    assertEquals("+", value.name)
    assertEquals("+", value.value)
  }

  @Test
  def testEnumValues(): Unit = {

    val partDsl = """+, -""".stripMargin


    val bla = sut.parsePartDsl(partDsl, sut.enumValues)

    val value = bla.toOption.get;

    val firstValue = value.head;

    assertNotNull(firstValue)
    assertEquals("+", firstValue.name)
    assertEquals("+", firstValue.value)

    val secondValue = value(1);
    assertNotNull(firstValue)
    assertEquals("-", secondValue.name)
    assertEquals("-", secondValue.value)

  }

  @Test
  def testEnum(): Unit = {

    val c0RecordDsl =
      """Record baseAmount {
        |     Field sign:                    C          pos (45,  1) as enum { +, - }
        |}""".stripMargin


    val c0Record = sut.parseDSL(c0RecordDsl)


    assertTrue(c0Record.isRight, c0Record.fold(s => s, _ => ""))
    val value = c0Record.toOption.get;

    assertEquals("baseAmount", value.name)
  }

  @Test
  def testComplexDSL(): Unit = {

    val c0RecordDsl =
      """Record baseAmount {
        |     Field required variety:        C          pos ( 1,  1) as fixedValue C
        |     Field required type:           C          pos (42,  1) as fixedValue 0
        |
        |     Field required conditionId:    Zc         pos ( 2,  3) as fixedValue 000
        |
        |     Field sign:                    C          pos (45,  1) as enum { +, - }
        |     Field amount:                  N (10, 5)  pos (46, 15)
        |}""".stripMargin


    val c0Record = sut.parseDSL(c0RecordDsl)


    assertTrue(c0Record.isRight, c0Record.fold(s => s, _ => ""))
    val record = c0Record.toOption.get;

    assertEquals("baseAmount", record.name)

    assertEquals("variety",     record.fields(0).name)
    assertTrue(                          record.fields(0).required)
    assertEquals("C",           record.fields(0).dataType.name)
    assertEquals(1,             record.fields(0).dataType.position)
    assertEquals(1,             record.fields(0).dataType.length)
    assertEquals("C",           record.fields(0).dataType.fixedValue.get)

    assertEquals("type",        record.fields(1).name)
    assertTrue(                          record.fields(1).required)
    assertEquals("C",           record.fields(1).dataType.name)
    assertEquals(42,            record.fields(1).dataType.position)
    assertEquals(1,             record.fields(1).dataType.length)
    assertEquals("0",           record.fields(1).dataType.fixedValue.get)

    assertEquals("conditionId", record.fields(2).name)
    assertTrue(                          record.fields(2).required)
    assertEquals("Zc",          record.fields(2).dataType.name)
    assertEquals(2,             record.fields(2).dataType.position)
    assertEquals(3,             record.fields(2).dataType.length)
    assertEquals("000",         record.fields(2).dataType.fixedValue.get)

    assertEquals("sign",        record.fields(3).name)
    assertFalse(                         record.fields(3).required)
    assertEquals("C",           record.fields(3).dataType.name)
    assertEquals(45,            record.fields(3).dataType.position)
    assertEquals(1,             record.fields(3).dataType.length)
    assertTrue(                          record.fields(3).dataType.fixedValue.isEmpty)
    assertEquals("+",           record.fields(3).dataType.enumeration.get.values(0).name)
    assertEquals("+",           record.fields(3).dataType.enumeration.get.values(0).value)
    assertEquals("-",           record.fields(3).dataType.enumeration.get.values(1).name)
    assertEquals("-",           record.fields(3).dataType.enumeration.get.values(1).value)

    assertEquals("amount",      record.fields(4).name)
    assertFalse(                         record.fields(4).required)
    assertEquals("N",           record.fields(4).dataType.name)
    assertEquals(10,            record.fields(4).dataType.floatDigitsBeforeDecimal.get)
    assertEquals(5,             record.fields(4).dataType.floatDigitsAfterDecimal.get)
    assertEquals(46,            record.fields(4).dataType.position)
    assertEquals(15,            record.fields(4).dataType.length)
    assertTrue(                          record.fields(4).dataType.fixedValue.isEmpty)
  }
}
