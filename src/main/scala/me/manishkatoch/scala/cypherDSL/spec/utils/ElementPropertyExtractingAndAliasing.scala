package me.manishkatoch.scala.cypherDSL.spec.utils

import me.manishkatoch.scala.cypherDSL.spec.entities.Node
import me.manishkatoch.scala.cypherDSL.spec.Utils._

private[cypherDSL] trait ElementPropertyExtractingAndAliasing {
  private val tooManyPropertiesToAliasMessage = "Alias one property at a time!"

  def getElementAndProperties(element: Product): (Product, List[String]) =
    element match {
      case s: Node[_, _] => (s.element, s.properties.toList[Symbol].map(_.name))
      case s             => (s, List.empty[String])
    }

  @throws[AssertionError]
  def makeAliasedString(identifier: String, properties: List[String], alias: Option[String]): String = {
    if (properties.length > 1 && alias.isDefined) {
      throw new AssertionError(tooManyPropertiesToAliasMessage)
    }
    val identifierString = properties.map(p => s"$identifier.$p").mkString(",")
    val aliasString      = alias.map(a => s" as $a").mkString
    (if (identifierString.isEmpty) identifier else identifierString) + aliasString
  }
}
