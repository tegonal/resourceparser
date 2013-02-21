package com.tegonal.resourceparser

import scala.util.parsing.combinator.RegexParsers

class ResourceParser extends RegexParsers {

  /**
   * Convinient entry method
   */
  def parse(input: String) = parseAll(resourceBundle, input)

  /**
   * The top level entry point
   */
  def resourceBundle: Parser[ResourceBundle] =
    rep(property) ^^ (x => ResourceBundle(x))

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

  def pathElement: Parser[PathElement] =
    """(\w*)""".r ^^ (x => PathElement(x))

  def value: Parser[PropertyValue] =
    """(.*)""".r ^^ (x => PropertyValue(x))
}