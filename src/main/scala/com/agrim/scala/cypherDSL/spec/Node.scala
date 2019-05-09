package com.agrim.scala.cypherDSL.spec

import com.agrim.scala.cypherDSL.spec.implicits.QueryProvider
import shapeless.ops.hlist.ToTraversable
import shapeless.{HList, HNil}

private[spec] sealed abstract class CypherEntity[T <: Product: QueryProvider, H <: HList](element: T, properties: H)(
    implicit i0: ToTraversable.Aux[H, List, Symbol]) {

  private val queryProvider = implicitly[QueryProvider[T]]

  def toQuery(context: Context = new Context()): String = {
    implicit val lpContext = context
    context
      .map(element)(getIdentifierOnlyQuery)
      .getOrElse {
        val id = context.add(element)
        getQueryBasedOnProperties(id)
      }
  }
  private def getQueryBasedOnProperties(id: String)(implicit context: Context) = {
    val matchers = properties match {
      case _: HNil => queryProvider.getMatchers(element)
      case _       => queryProvider.getMatchers(element, properties)
    }
    makeExpandedQuery(id, matchers)
  }

  private def getIdentifierOnlyQuery(id: String): String = makeQuery(id)

  private def makeExpandedQuery(id: String, parts: Seq[String]) = {
    val repr = s"$id:${element.getClass.getSimpleName} {${parts.mkString(",")}}"
    makeQuery(repr)
  }

  private def makeQuery(repr: String) = s"$repr"
}

private[cypherDSL] case class Node[T <: Product: QueryProvider, H <: HList](element: T, properties: H)(
    implicit i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherEntity(element, properties) {
  override def toQuery(context: Context = new Context()): String = s"(${super.toQuery(context)})"
}
private[cypherDSL] case class Relationship[T <: Product: QueryProvider, H <: HList](element: T, properties: H)(
    implicit
    i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherEntity(element, properties) {
  override def toQuery(context: Context = new Context()): String = s"[${super.toQuery(context)}]"
}
