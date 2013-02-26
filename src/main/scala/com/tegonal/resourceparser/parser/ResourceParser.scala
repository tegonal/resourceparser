package com.tegonal.resourceparser.parser

import scala.util.parsing.combinator._

class ResourceParser extends JavaTokenParsers {

  override def skipWhitespace = false

  /**
   * Convenient entry method
   */
  def parse(input: String): ParseResult[ResourceBundle] = parseAll(resourceBundle, input)

  /**
   * The top level entry point
   */
  def resourceBundle: Parser[ResourceBundle] =
    repsep(property | comment, whiteSpace) ^^ (x => ResourceBundle(x))

  /**
   * Comment of the form #comment text
   */
  def comment: Parser[Comment] =
    "#" ~> """([^\n\r]+)""".r ^^ { case text => Comment(text) }

  /**
   * Property of the form path=value
   */
  def property: Parser[Property] =
    path ~ """\s*=""".r ~ value ^^ { case lhs ~ eq ~ rhs => Property(lhs, rhs) }

  /**
   * Path of the form path.path.path
   */
  def path: Parser[Path] =
    pathElement ~ rep("." ~> pathElement) ^^ { case lhs ~ rhs => Path(lhs :: rhs) }

  /**
   * Path elements must consist of word characters
   */
  def pathElement: Parser[PathElement] =
    """(\w+)""".r ^^ (x => PathElement(x))

  /**
   * Every character is allowed except new lines
   */
  def value: Parser[PropertyValue] =
    """([^\n\r]*)""".r ^^ (x => PropertyValue(x))
}

object ResourceParser {

  /**
   * @param input multi-line string of the resource file
   * @return the resulting AST if the parsing was successful, else None.
   */
  def parse(input: String): Option[ResourceBundle] =
    (new ResourceParser).parse(input).map { Some(_) }.getOrElse(None)

}