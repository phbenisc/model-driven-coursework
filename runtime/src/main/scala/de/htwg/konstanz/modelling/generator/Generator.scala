package de.htwg.konstanz.modelling.generator

import de.htwg.konstanz.modelling.parser.Parser
import de.htwg.konstanz.modelling.parser.Transformer
import de.htwg.konstanz.modelling.templates.xml.Records

import scala.reflect.io.File
import scala.xml.{PrettyPrinter, XML}

class Generator {

  def generateFile(dsl: String): Unit = {

    val parser = new Parser

    for(
      record <- Transformer.apply(parser.parseDSL(dsl))
    ) {
      val string = Records.render(record).toString.stripLeading()
      val xml = XML.loadString(string)
      val p = new PrettyPrinter(80, 4)

      File("record.xsd").writeAll(p.format(xml))
    }
  }
}
