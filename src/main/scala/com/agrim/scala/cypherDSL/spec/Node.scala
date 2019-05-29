package com.agrim.scala.cypherDSL.spec

import com.agrim.scala.cypherDSL.spec.entities.CypherEntity
import shapeless.ops.hlist.ToTraversable
import shapeless.{HList, HNil}

private[spec] sealed abstract class CypherInstantiated[T <: Product: QueryProvider, H <: HList](
    element: T,
    properties: H)(implicit i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherEntity {

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

  def toQuery: String = if (isEmpty) "" else s"$start..$end"
}

object CypherRange {
  val empty = CypherRange(0, 0)

  def apply(range: Range): CypherRange = CypherRange(range.start, range.end)
}

private[cypherDSL] case class Node[T <: Product: QueryProvider, H <: HList](element: T, properties: H)(
    implicit i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherInstantiated(element, properties) {
  override def toQuery(context: Context = new Context()): String = s"(${super.toQuery(context)})"
}

private[cypherDSL] case class VariableLengthRelationship[H <: HList](range: CypherRange, properties: H = HNil)(
    implicit
    queryProvider: QueryProvider[CypherRange],
    i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherInstantiated(range, properties) {
  override def toQuery(context: Context = new Context()): String = s"[*${range.toQuery}]"
}

private[cypherDSL] case class OrRelationship[T <: Product: QueryProvider, H <: HList](element: T, properties: H)(
    implicit i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherInstantiated(element, properties)
    with UpperSnakeCasing {
  override def toQuery(context: Context = new Context()): String = s"${super.toQuery(context)}"

  override def label: String = upperSnakeCased(super.label)
}

private[cypherDSL] case class Relationship[T <: Product: QueryProvider, H <: HList](
    element: T,
    properties: H,
    orRels: List[OrRelationship[_, _]] = List.empty)(implicit
                                                     i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherInstantiated(element, properties)
    with UpperSnakeCasing {

  override def toQuery(context: Context = new Context()): String = {
    val orRelsString = orRels.map(_.toQuery(context)).mkString("|")
    s"[${super.toQuery(context)}${if (orRelsString.nonEmpty) "|" + orRelsString else ""}]"
  }

  override def label: String = upperSnakeCased(super.label)

  def or[U <: Product: QueryProvider, UH <: HList](orElement: U, properties: UH)(
      implicit i1: ToTraversable.Aux[UH, List, Symbol]) = {
    copy(orRels = OrRelationship(orElement, properties) :: orRels)
  }
}

private[spec] trait UpperSnakeCasing {
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

  def upperSnakeCased(string: String): String = toUpperSnakeCase(string)
}
