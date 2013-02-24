package com.tegonal.resourceparser.generator

object ResourceImplicitsTest {
  /**
   * Definitions
   */
  abstract class PathElement(val identifier: String)

  trait ResourcePath {
    def pathElements: Seq[PathElement]

    def resourceString = pathElements.map(_.identifier).mkString(".")
  }

  case object Items extends PathElement("items") {
    def list = ItemsList
  }

  case object ItemsList extends PathElement("list") {
    def title = ItemsListTitle
  }

  case object ItemsListTitle extends PathElement("title") with ResourcePath {
    def pathElements = Items :: ItemsList :: ItemsListTitle :: Nil

    def details = ItemsListTitleDetails
  }

  case object ItemsListTitleDetails extends PathElement("details") with ResourcePath {
    def pathElements = Items :: ItemsList :: ItemsListTitle :: this :: Nil
  }

  def items = Items //> items: => org.rawyler.worksheets.ResourceImplicits.Items.type

  /**
   * implicit conversion from resource path to string
   */
  implicit def resourcePath2String(resourcePath: ResourcePath): String =
    resourcePath.resourceString //> resourcePath2String: (resourcePath: org.rawyler.worksheets.ResourceImplicit
  //| s.ResourcePath)String

  // tests
  val a: String = items.list.title //> a  : String = items.list.title
  val b: String = items.list.title.details //> b  : String = items.list.title.details
}