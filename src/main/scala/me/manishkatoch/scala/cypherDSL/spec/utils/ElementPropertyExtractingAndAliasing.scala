package me.manishkatoch.scala.cypherDSL.spec.utils

import me.manishkatoch.scala.cypherDSL.spec.entities.{CypherType, Node, NodeType, RelationType}
import me.manishkatoch.scala.cypherDSL.spec.Utils._

private[cypherDSL] trait ElementPropertyExtracting {
  def getElementAndProperties(element: Product): (Any, List[String]) =
    element match {
      case s: Node[_, _]   => (s.element, s.properties.toList[Symbol].map(_.name))
      case s: RelationType => (s.fingerprint, List.empty[String])
      case s: NodeType     => (s.fingerprint, List.empty[String])
      case s               => (s, List.empty[String])
    }
}

private[cypherDSL] trait ElementPropertyAliasing {
  private val tooManyPropertiesToAliasMessage = "Alias one property at a time!"

  @throws[AssertionError]
  def makeAliasedString(identifier: String, properties: List[String], alias: Option[String] = None): String = {
    if (properties.length > 1 && alias.isDefined) {
      throw new AssertionError(tooManyPropertiesToAliasMessage)
    }
    val identifierString = properties.map(p => s"$identifier.$p").mkString(",")
    val aliasString      = alias.map(a => s" as $a").mkString
    (if (identifierString.isEmpty) identifier else identifierString) + aliasString
  }
}

private[cypherDSL] trait ElementPropertyExtractingAndAliasing
  extends ElementPropertyExtracting with ElementPropertyAliasing {}
