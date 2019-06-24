package me.manishkatoch.scala.cypherDSL.spec.entities

import me.manishkatoch.scala.cypherDSL.spec.utils.SnakeCasing
import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult, QueryProvider}
import shapeless.ops.hlist.ToTraversable
import shapeless.{HList, HNil}

private[spec] sealed abstract class CypherInstance[T <: Product: QueryProvider, H <: HList](element: T, properties: H)(
    implicit i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherEntity {

  private val queryProvider = implicitly[QueryProvider[T]]

  def toQuery(context: Context = new Context()): DSLResult = {
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

  private def makeExpandedQuery(id: String, parts: Seq[DSLResult]) = {
    val (query, paramMap) = parts.foldLeft((List.empty[String], Map.empty[String, Any])) { (acc, part) =>
      (acc._1 :+ part.query, acc._2 ++ part.queryMap)
    }
    val repr = s"$id:$label {${query.mkString(",")}}"
    DSLResult(repr, paramMap)
  }

  def label: String = element.getClass.getSimpleName

  private def getIdentifierOnlyQuery(id: String): DSLResult = DSLResult(id)

}

private[cypherDSL] case class Node[T <: Product: QueryProvider, H <: HList](element: T, properties: H)(
    implicit i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherInstance(element, properties) {
  override def toQuery(context: Context = new Context()): DSLResult = {
    val result = super.toQuery(context)
    result.copy(query = s"(${result.query})")
  }
}

private[cypherDSL] case class Relationship[T <: Product: QueryProvider, H <: HList](
    element: T,
    properties: H,
    variableLengthRelation: Option[VariableLengthRelation] = None,
    orRelations: List[RelationTypeOrInstance] = List.empty)(implicit
                                                            i0: ToTraversable.Aux[H, List, Symbol])
    extends CypherInstance(element, properties)
    with SnakeCasing {

  override def toQuery(context: Context = new Context()): DSLResult = {
    val (orRelationString, orRelationMap) = if (context.get(element).isDefined) {
      ("", Map.empty)
    } else {
      val orRelationsResults    = orRelations.map(_.toQuery(context))
      val orRelationStringIfAny = orRelationsResults.map(result => s"|:${result.query}").mkString
      val orRelationMapIfAny = orRelationsResults.foldLeft(Map.empty[String, Any]) { (acc, result) =>
        acc ++ result.queryMap
      }
      (orRelationStringIfAny, orRelationMapIfAny)
    }

    val varLengthStringIfAny = variableLengthRelation.map(_.toQuery(context)).mkString
    val result               = super.toQuery(context)
    result.copy(query = s"[${result.query}$orRelationString$varLengthStringIfAny]",
                queryMap = result.queryMap ++ orRelationMap)
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
  override def toQuery(context: Context): DSLResult = {
    DSLResult(s"[${variableLengthRelation.toQuery(context)}]")
  }
}
