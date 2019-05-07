package com.agrim.scala.cypherDSL.spec

import shapeless.HList

private[cypherDSL] case class PathLink[T <: Product, H <: HList](leftLink: Option[String],
                                                                 element: CypherEntity[T, H],
                                                                 rightLink: Option[String]) {
  def toQuery(implicit context: Context): String =
    leftLink.map(_.toString).mkString + element.toQuery + rightLink.map(_.toString).mkString
}
