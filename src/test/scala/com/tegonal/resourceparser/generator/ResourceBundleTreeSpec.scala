package com.tegonal.resourceparser.generator

import org.specs2.mutable._
import com.tegonal.resourceparser.parser._

class ResourceBundleTreeSpec extends Specification {
  val emptyResourceBundle = ResourceBundle(Nil)

  val singlePropertyResourceBundle = ResourceBundle(
    Property(
      Path(PathElement("items") :: PathElement("list") :: PathElement("title") :: Nil), PropertyValue("")) :: Nil)

  val multiplePropertiesResourceBundle = ResourceBundle(
    Property(Path(PathElement("items") :: PathElement("list") :: PathElement("title") :: Nil), PropertyValue("Items")) ::
      Property(Path(PathElement("items") :: PathElement("list") :: PathElement("details") :: Nil), PropertyValue("Items detailed")) ::
      Property(Path(PathElement("orders") :: PathElement("title") :: Nil), PropertyValue("Order")) ::
      Nil)

  val nestedPropertiesResourceBundle = ResourceBundle(
    Property(Path(PathElement("items") :: PathElement("list") :: PathElement("title") :: Nil), PropertyValue("Items")) ::
      Property(Path(PathElement("items") :: PathElement("list") :: PathElement("title") :: PathElement("additional") :: Nil), PropertyValue("Items detailed")) ::
      Property(Path(PathElement("orders") :: PathElement("title") :: Nil), PropertyValue("Order")) ::
      Nil)

  "A tree" should {
    "be created for an empty property" in {
      ResourceBundleTree.create(emptyResourceBundle) === ResourceNode(Nil, Nil)
    }

    "be created for a simple property" in {
      ResourceBundleTree.create(singlePropertyResourceBundle) === ResourceNode(Nil,
        ResourceNode("items" :: Nil,
          ResourceNode("items" :: "list" :: Nil,
            ResourceNode("items" :: "list" :: "title" :: Nil, Nil, true) :: Nil) :: Nil) :: Nil)
    }

    "be created for multiple properties" in {
      ResourceBundleTree.create(multiplePropertiesResourceBundle) === ResourceNode(Nil,
        ResourceNode("items" :: Nil,
          ResourceNode("items" :: "list" :: Nil,
            ResourceNode("items" :: "list" :: "title" :: Nil, Nil, true) :: ResourceNode("items" :: "list" :: "details" :: Nil, Nil, true) :: Nil) :: Nil) ::
          ResourceNode("orders" :: Nil,
            ResourceNode("orders" :: "title" :: Nil, Nil, true) :: Nil) :: Nil)
    }

    "be created for nested properties" in {
      ResourceBundleTree.create(nestedPropertiesResourceBundle) === ResourceNode(Nil,
        ResourceNode("items" :: Nil,
          ResourceNode("items" :: "list" :: Nil,
            ResourceNode("items" :: "list" :: "title" :: Nil, ResourceNode("items" :: "list" :: "title" :: "additional" :: Nil, Nil, true) :: Nil, true) :: Nil) :: Nil) ::
          ResourceNode("orders" :: Nil,
            ResourceNode("orders" :: "title" :: Nil, Nil, true) :: Nil) :: Nil)
    }
  }
}