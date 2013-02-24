package com.tegonal.resourceparser.generator

import com.tegonal.resourceparser.parser._

object ResourceToScalaGenerator {

  def generateSource(input: String): Option[String] = {
    ResourceParser.parse(input) map {
      // TODO create tree form with a targetAST and then generate
      generate(_)
    }
  }

  def generate(resourceComponent: ResourceComponent): String = resourceComponent match {
    case ResourceBundle(properties) => "resource bundle { " + properties.map(generate(_)) + "}"
    case Property(path, propertyValue) => s"""case object ${path.pathElements.map { _.name.capitalize }.mkString} extends PathElement("${path.pathElements.last.name}") with ResourcePath {
                                            |def pathElements = ${path.pathElements.map { _.name.capitalize }.mkString("::")} :: Nil
                                            |
                                            |// TODO def further = ???
                                            |}""".stripMargin
    case _ => "was something else"
  }
}