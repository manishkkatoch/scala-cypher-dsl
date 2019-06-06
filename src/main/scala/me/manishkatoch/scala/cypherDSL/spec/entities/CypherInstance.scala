package me.manishkatoch.scala.cypherDSL.spec.entities

import me.manishkatoch.scala.cypherDSL.spec.utils.SnakeCasing
import me.manishkatoch.scala.cypherDSL.spec.{Context, QueryProvider}
import shapeless.ops.hlist.ToTraversable
import shapeless.{HList, HNil}

private[spec] sealed abstract class CypherInstance[T <: Product: QueryProvider, H <: HList](element: T, properties: H)(
    implicit i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherEntity {

  private val queryProvider = implicitly[QueryProvider[T]]

  def toQuery(context: Context = new Context()): String = {
    context.map(element)(getIdentifierOnlyQuery).getOrElse {
      val id = context.add(element)
      getQueryBasedOnProperties(id, context)
    }
  }

  private def getQueryBasedOnProperties(id: String, context: Context) = {
    implicit val lowPriorityContext: Context = context
    val matchers = properties match {
      case _: HNil => queryProvider.getMatchers(element)
      case _       => queryProvider.getMatchers(element, properties)
    }
    makeExpandedQuery(id, matchers)
  }

  private def makeExpandedQuery(id: String, parts: Seq[String]) = {
    val repr = s"$id:$label {${parts.mkString(",")}}"
    makeQuery(repr)
  }

  def label: String = element.getClass.getSimpleName

  private def getIdentifierOnlyQuery(id: String): String = makeQuery(id)

  private def makeQuery(repr: String) = s"$repr"
}

private[cypherDSL] case class Node[T <: Product: QueryProvider, H <: HList](element: T, properties: H)(
    implicit i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherInstance(element, properties) {
  override def toQuery(context: Context = new Context()): String = s"(${super.toQuery(context)})"
}

private[cypherDSL] case class Relationship[T <: Product: QueryProvider, H <: HList](
    element: T,
    properties: H,
    variableLengthRelation: Option[VariableLengthRelation] = None,
    orRelations: List[RelationTypeOrInstance] = List.empty)(implicit
                                                            i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherInstance(element, properties)
    with SnakeCasing {

  override def toQuery(context: Context = new Context()): String = {
    val orRelationStringIfAny =
      if (context.get(element).isDefined) "" else orRelations.map(_.toQuery(context)).map(str => s"|:$str").mkString
    val varLengthStringIfAny = variableLengthRelation.map(_.toQuery(context)).mkString
    s"[${super.toQuery(context)}$orRelationStringIfAny$varLengthStringIfAny]"
  }

  def or[U <: Product, UH <: HList](rel: U, properties: UH)(
      implicit queryProvider: QueryProvider[U],
      i1: ToTraversable.Aux[UH, List, Symbol]): Relationship[T, H] = {
    val relationTypeOrInstance = RelationTypeOrInstance(Relationship(rel, properties))
    copy(orRelations = orRelations :+ relationTypeOrInstance)
  }
  def or(rel: RelationType): Relationship[T, H] = {
    val relationTypeOrInstance = RelationTypeOrInstance(rel)
    copy(orRelations = orRelations :+ relationTypeOrInstance)
  }

  override def label: String = upperSnakeCased(super.label)
}

private[cypherDSL] case class VariableLengthRelationship(variableLengthRelation: VariableLengthRelation)
    extends CypherEntity {
  override def toQuery(context: Context): String = s"[${variableLengthRelation.toQuery(context)}]"
}
