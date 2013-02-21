package com.tegonal.resourceparser

case class ResourceBundle(val properties: Seq[Property])

case class Path(val pathElements: Seq[PathElement])

case class PathElement(val name: String)

case class PropertyValue(val value: String)

case class Property(val path: Path, val value: PropertyValue)

case class ResourceNode(val name: PathElement, val children: Seq[ResourceNode], val value: Option[PropertyValue])