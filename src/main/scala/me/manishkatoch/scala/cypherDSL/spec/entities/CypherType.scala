package me.manishkatoch.scala.cypherDSL.spec.entities

import java.util.UUID

import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult, QueryProvider}
import me.manishkatoch.scala.cypherDSL.spec.utils.SnakeCasing
import shapeless.HList
import shapeless.ops.hlist.ToTraversable

import scala.reflect.runtime.universe.{typeOf, Type}

private[spec] sealed abstract class CypherType(tpe: Type, fingerprint: UUID) extends CypherEntity {
  def toQuery(context: Context = new Context()): DSLResult = {
    context
      .map(fingerprint) { result =>
        DSLResult(result.trim)
      }
      .getOrElse {

        val id = context.add(fingerprint)
        val labelString = tpe match {
          case s if s =:= typeOf[Any] => ""
          case _                      => s":$label"
        }
        DSLResult(s"$id$labelString")
      }
  }

  def label: String = tpe.typeSymbol.asClass.name.decodedName.toString
}

private[cypherDSL] case class NodeType(tpe: Type) extends CypherType(tpe, UUID.randomUUID()) {
  override def toQuery(context: Context): DSLResult = {
    val result = super.toQuery(context)
    result.copy(query = s"(${result.query})")
  }
}

private[cypherDSL] case class RelationType(tpe: Type,
                                           variableLengthRelation: Option[VariableLengthRelation] = None,
                                           orRelations: List[RelationTypeOrInstance] = List.empty)
    extends CypherType(tpe, UUID.randomUUID())
    with SnakeCasing {

  override def toQuery(context: Context): DSLResult = {
    val orRelationStringIfAny =
      if (context.get(tpe).isDefined) ""
      else orRelations.map(_.toQuery(context)).map(result => s"|:${result.query}").mkString
    val varLengthStringIfAny = variableLengthRelation.map(_.toQuery(context)).mkString
    val result               = super.toQuery(context)
    result.copy(query = s"[${result.query}$orRelationStringIfAny$varLengthStringIfAny]")
  }

  def or[U <: Product, UH <: HList](rel: U, properties: UH)(implicit queryProvider: QueryProvider[U],
                                                            i1: ToTraversable.Aux[UH, List, Symbol]): RelationType = {
    val relationTypeOrInstance = RelationTypeOrInstance(Relationship(rel, properties))
    copy(orRelations = orRelations :+ relationTypeOrInstance)
  }
  def or(rel: RelationType): RelationType = {
    val relationTypeOrInstance = RelationTypeOrInstance(rel)
    copy(orRelations = orRelations :+ relationTypeOrInstance)
  }
  override def label: String = upperSnakeCased(super.label)
}

object RelationType {
  def apply(tpe: Type): RelationType = new RelationType(tpe, None)
  def apply(tpe: Type, variableLengthRelation: VariableLengthRelation): RelationType =
    new RelationType(tpe, Option(variableLengthRelation))
}
