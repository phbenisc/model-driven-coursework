package de.htwg.konstanz.modelling.generator

import java.nio.file.{Files, Paths}

import de.htwg.konstanz.modelling.parser.Parser
import de.htwg.konstanz.modelling.parser.Transformer
import de.htwg.konstanz.modelling.templates.xml.Records

import scala.io.Source
import scala.reflect.io.File
import scala.xml.{PrettyPrinter, XML}

object Generator {

  def main(args: Array[String]): Unit = {

    val filename = args(0)
    val path = Paths.get(filename).toAbsolutePath

    val file = path.toFile
    val source = Source.fromFile(file)

    val fileContents = source.getLines
      .mkString

    source.close

    val outputPath = Paths.get(args(1))
    val xsdFilename = outputPath.resolve(file.getName.stripSuffix(".dsl") + ".xsd").toAbsolutePath.toString
    Files.createDirectories(outputPath)

    File(xsdFilename).writeAll(generate(fileContents))
  }

  def generateFile(dsl: String): Unit = {
    File("record.xsd").writeAll(generate(dsl))
  }

  def generate(dsl: String): String = {

    val parser = new Parser

    val myRecords = for (
      record <- Transformer.apply(parser.parseDSL(dsl))
    ) yield record

    myRecords
      .map(schema => Records.render(schema))
      .map(schema => schema.toString())
      .map(schemaStr => schemaStr.stripLeading())
      .map(schemaStr => XML.loadString(schemaStr))
      .map(schemaDom => new PrettyPrinter(120, 4).format(schemaDom))
      .fold(s => s, s => s)
  }
}
