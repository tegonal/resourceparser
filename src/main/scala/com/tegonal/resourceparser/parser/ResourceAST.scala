package com.tegonal.resourceparser.parser

trait ResourceComponent

case class ResourceBundle(val properties: List[Property]) extends ResourceComponent

case class Path(val pathElements: List[PathElement]) extends ResourceComponent

case class PathElement(val name: String) extends ResourceComponent

case class PropertyValue(val value: String) extends ResourceComponent

case class Property(val path: Path, val value: PropertyValue) extends ResourceComponent