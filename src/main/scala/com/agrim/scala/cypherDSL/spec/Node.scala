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

  def label: String = element.getClass.getSimpleName

  private def getQueryBasedOnProperties(id: String)(implicit context: Context) = {
    val matchers = properties match {
      case _: HNil => queryProvider.getMatchers(element)
      case _       => queryProvider.getMatchers(element, properties)
    }
    makeExpandedQuery(id, matchers)
  }

  private def getIdentifierOnlyQuery(id: String): String = makeQuery(id)

  private def makeExpandedQuery(id: String, parts: Seq[String]) = {
    val repr = s"$id:$label {${parts.mkString(",")}}"
    makeQuery(repr)
  }

  private def makeQuery(repr: String) = s"$repr"
}

private[cypherDSL] case class CypherRange(start: Int, end: Int) {
  def isEmpty: Boolean = (end - start) == 0
  def toQuery: String  = if (isEmpty) "" else s"$start..$end"
}
object CypherRange {
  val empty                            = CypherRange(0, 0)
  def apply(range: Range): CypherRange = CypherRange(range.start, range.end)
}
private[cypherDSL] case class Node[T <: Product: QueryProvider, H <: HList](element: T, properties: H)(
    implicit i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherEntity(element, properties) {
  override def toQuery(context: Context = new Context()): String = s"(${super.toQuery(context)})"
}

private[cypherDSL] case class VariableLengthRelationship[H <: HList](range: CypherRange, properties: H = HNil)(
    implicit
    queryProvider: QueryProvider[CypherRange],
    i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherEntity(range, properties) {
  override def toQuery(context: Context = new Context()): String = s"[*${range.toQuery}]"
}

private[cypherDSL] case class Relationship[T <: Product: QueryProvider, H <: HList](element: T, properties: H)(
    implicit
    i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherEntity(element, properties) {
  override def toQuery(context: Context = new Context()): String = s"[${super.toQuery(context)}]"
  private def toUpperSnakeCase(label: String): String = {
    label
      .flatMap(char => {
        if (char.isUpper) Seq("_", char)
        else Seq(char)
      })
      .mkString
      .stripPrefix("_")
      .toUpperCase
  }
  override def label: String = toUpperSnakeCase(super.label)

}
