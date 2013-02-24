package com.tegonal.resourceparser.parser

import org.specs2.mutable._

class ResourceParserSpec extends Specification {

  val resourceParser = new ResourceParser

  "Resource file parser" should {
    "succeed with simple properties" in {
      resourceParser.parse("items=Items").successful === true
    }

    "succeed with detail properties" in {
      resourceParser.parse("items.details.title=This is the detail title").successful === true
    }

    "fail without equals sign" in {
      resourceParser.parse("items.details.titleThis is the detail title").successful === false
    }

    "succeed with two equals as property values allow any character" in {
      resourceParser.parse("items.details.title==This is the detail title").successful === true
    }

    "result in resource file ast" in {
      resourceParser.parse("items=Items").get === ResourceBundle(Property(Path(PathElement("items") :: Nil), PropertyValue("Items")) :: Nil)
    }

    "succeed with multi lines" in {
      resourceParser.parse("""items=Items
                             |items.details.title=This is the detail title
                             |
                             |users=Users
                             |users.details.title=User Details""".stripMargin).get ===
        ResourceBundle(
          Property(Path(PathElement("items") :: Nil), PropertyValue("Items")) ::
            Property(Path(PathElement("items") :: PathElement("details") :: PathElement("title") :: Nil), PropertyValue("This is the detail title")) ::
            Property(Path(PathElement("users") :: Nil), PropertyValue("Users")) ::
            Property(Path(PathElement("users") :: PathElement("details") :: PathElement("title") :: Nil), PropertyValue("User Details")) ::
            Nil)

    }

  }

}
