package com.tegonal.resourceparser.generator

case class ResourceNode(val path: Seq[String], val children: List[ResourceNode], val isProperty: Boolean = false)