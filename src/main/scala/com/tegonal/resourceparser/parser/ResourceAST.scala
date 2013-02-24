package com.tegonal.resourceparser.parser

case class ResourceBundle(val properties: Seq[Property])

case class Path(val pathElements: Seq[PathElement])

case class PathElement(val name: String)

case class PropertyValue(val value: String)

case class Property(val path: Path, val value: PropertyValue)
