package com.tegonal.resourceparser.generator

import org.specs2.mutable._
import com.tegonal.resourceparser.parser.ResourceParser

class ResourceToScalaGeneratorSpec extends Specification {
  val resourceFile = """items.details=Item details
                       |items.list.title=Items
                       |orders.list.title=Orders
                       |orders.details.title=Order""".stripMargin

  "The generator" should {
    "generate Scala source code" in {
      val result = ResourceToScalaGenerator.generate(ResourceParser.parse(resourceFile).get)
      result must not beEmpty
    }
  }
}