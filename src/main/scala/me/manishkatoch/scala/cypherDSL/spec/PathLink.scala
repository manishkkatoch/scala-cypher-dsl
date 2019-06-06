package me.manishkatoch.scala.cypherDSL.spec

import me.manishkatoch.scala.cypherDSL.spec.entities.CypherEntity

private[cypherDSL] case class PathLink(leftLink: Option[String], element: CypherEntity, rightLink: Option[String]) {
  def toQuery(context: Context = new Context()): String =
    leftLink.map(_.toString).mkString + element.toQuery(context) + rightLink.map(_.toString).mkString
}
