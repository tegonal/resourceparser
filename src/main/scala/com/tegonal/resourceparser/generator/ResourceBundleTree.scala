package com.tegonal.resourceparser.generator

import com.tegonal.resourceparser.parser._
import scala.collection.mutable.ListBuffer

class ResourceBundleTree {

  val resources: scala.collection.mutable.Map[Path, ResourceNodeBuffer] = new scala.collection.mutable.HashMap

  def create(resourceComponent: ResourceComponent): ResourceNode = {
    resourceComponent match {
      case ResourceBundle(properties) => {
        resources.put(Path(Nil), ResourceNodeBuffer(Nil, ListBuffer.empty))

        properties.map(createProperty(_))
      }
    }

    resources(Path(Nil)).toResourceNode
  }

  // TODO simplify
  def createProperty(property: Property) = property match {
    case Property(path, _) =>
      // ResourceNode already exists
      resources.get(path).map { existing =>
        // just update the path
        existing.isProperty = true
      }.getOrElse {
        // create the ResourceNode
        val created = ResourceNodeBuffer(path.pathElements.map(_.name), ListBuffer.empty, true)
        resources.put(path, created)
        // add child to parent
        resources.get(Path(path.pathElements.init)).map { parent =>
          parent.children += created
        }.getOrElse {
          createHierarchy(Path(path.pathElements.init), created)
        }
      }
  }

  def createHierarchy(path: Path, child: ResourceNodeBuffer): Unit = path match {
    case Path(pathElements) => {
      resources.get(path).map { existing =>
        existing.children += child
      }.getOrElse {
        val created = ResourceNodeBuffer(pathElements.map(_.name), ListBuffer.empty += child)
        resources.put(path, created)
        createHierarchy(Path(pathElements.init), created)
      }
    }
  }
}

object ResourceBundleTree {
  def create(resourceComponent: ResourceComponent): ResourceNode = (new ResourceBundleTree).create(resourceComponent)
}

case class ResourceNodeBuffer(val path: Seq[String], val children: scala.collection.mutable.ListBuffer[ResourceNodeBuffer], var isProperty: Boolean = false) {
  def toResourceNode: ResourceNode = ResourceNode(path, children.toList.map(_.toResourceNode), isProperty)
}