package com.tegonal.resourceparser.generator

import com.tegonal.resourceparser.parser._

object ResourceToScalaGenerator {

  def generateSource(input: String, packageName: String = "com.tegonal.resourceparser", objectName: String = "ResourceBundleImplicits"): Option[String] = {
    ResourceParser.parse(input) map { parsed =>
      s"""${open(packageName, objectName)}
         |${generate(ResourceBundleTree.create(parsed))}
         |${close}""".stripMargin
    }
  }

  def generate(resourceNode: ResourceNode): String = resourceNode match {
    case ResourceNode(Nil, children, false) => children.map(generate).mkString("\n")

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

  def open(packageName: String, objectName: String) = s"""package $packageName
                |
                |object $objectName {
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