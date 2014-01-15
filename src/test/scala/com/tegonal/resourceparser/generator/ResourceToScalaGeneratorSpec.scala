package com.tegonal.resourceparser.generator

import org.specs2.mutable._
import com.tegonal.resourceparser.parser.ResourceParser

class ResourceToScalaGeneratorSpec extends Specification {
  val resourceFile = """items.details=Item details
                       |items.list.title=Items
                       |orders.list.title=Orders
                       |
                       |
                       |orders.details.title=Order
                       |""".stripMargin

  val expected = """package com.tegonal.resourceparser
                   |
                   |object ResourceBundleImplicits {
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
                   |case object Items extends PathElement("items") {
                   |
                   |  def details = ItemsDetails
                   |
                   |  def list = ItemsList
                   |}
                   |
                   |def items = Items
                   |
                   |case object ItemsDetails extends PathElement("details") with ResourcePath {
                   |  def pathElements = Items :: ItemsDetails :: Nil
                   |
                   |}
                   |
                   |case object ItemsList extends PathElement("list") {
                   |
                   |  def title = ItemsListTitle
                   |}
                   |
                   |case object ItemsListTitle extends PathElement("title") with ResourcePath {
                   |  def pathElements = Items :: ItemsList :: ItemsListTitle :: Nil
                   |
                   |}
                   |
                   |case object Orders extends PathElement("orders") {
                   |
                   |  def list = OrdersList
                   |
                   |  def details = OrdersDetails
                   |}
                   |
                   |def orders = Orders
                   |
                   |case object OrdersList extends PathElement("list") {
                   |
                   |  def title = OrdersListTitle
                   |}
                   |
                   |case object OrdersListTitle extends PathElement("title") with ResourcePath {
                   |  def pathElements = Orders :: OrdersList :: OrdersListTitle :: Nil
                   |
                   |}
                   |
                   |case object OrdersDetails extends PathElement("details") {
                   |
                   |  def title = OrdersDetailsTitle
                   |}
                   |
                   |case object OrdersDetailsTitle extends PathElement("title") with ResourcePath {
                   |  def pathElements = Orders :: OrdersDetails :: OrdersDetailsTitle :: Nil
                   |
                   |}
                   |}""".stripMargin

  "The generator" should {
    "generate Scala source code" in {
      val result = ResourceToScalaGenerator.generateSource(resourceFile).get
      result.replaceAll("""[\n|\s]""", "") === expected.replaceAll("""[\n|\s]""", "")
    }
  }
}

