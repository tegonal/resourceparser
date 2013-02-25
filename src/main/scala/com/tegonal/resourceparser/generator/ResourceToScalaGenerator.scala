package com.tegonal.resourceparser.generator

import com.tegonal.resourceparser.parser._

object ResourceToScalaGenerator {

  def generateSource(input: String): Option[String] = {
    ResourceParser.parse(input) map { parsed =>
      generate(ResourceBundleTree.create(parsed))
    }
  }

  def generate(resourceNode: ResourceNode): String = resourceNode match {
    case ResourceNode(Nil, children, false) => s"""${open}
                                                  |${children.map(generate).mkString("\n")}
                                                  |${close}""".stripMargin

    case ResourceNode(path, children, isProperty) => createNodeCode(path, children, isProperty)
  }

  def createNodeCode(path: Seq[String], children: List[ResourceNode], isProperty: Boolean) = {
    s"""case object ${path.map { _.capitalize }.mkString} extends PathElement("${path.last}")${if (isProperty) " with ResourcePath" else ""} {
       |  ${if (isProperty) "def pathElements = " + path.zipWithIndex.map { case (p, i) => (0 to i).toList.map(path(_).capitalize).mkString }.mkString("::") + " :: Nil" else ""}
       |  ${children.map { c => "def " + c.path.last + " = " + c.path.map { _.capitalize }.mkString }.mkString("\n\n  ")}
       |}
       |${
      path match {
        case p :: Nil => "\ndef " + p + " = " + p.capitalize
        case _ => ""
      }
    }
       |
       |${children.map(generate).mkString}""".stripMargin
  }

  val open = s"""object ResourceBundleImplicits {
                |
                |/**
                | * Definitions
                | */
                |abstract class PathElement(val identifier: String)
                |
                |trait ResourcePath {
                |  def pathElements: Seq[PathElement]
                |
                |  def resourceString = pathElements.map(_.identifier).mkString(".")
                |}
                |
                |/**
                | * implicit conversion from resource path to string
                | */
                |implicit def resourcePath2String(resourcePath: ResourcePath): String =
                |  resourcePath.resourceString
                |
                |""".stripMargin

  val close = "}"
}