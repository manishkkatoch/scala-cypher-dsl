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

  override def toSetterQuery(context: Context): DSLResult = ???

  def label: String = tpe.typeSymbol.asClass.name.decodedName.toString
}

private[cypherDSL] case class NodeType(tpe: Type, fingerprint: UUID = UUID.randomUUID())
    extends CypherType(tpe, fingerprint) {
  override def toQuery(context: Context): DSLResult = {
    val result = super.toQuery(context)
    result.copy(query = s"(${result.query})")
  }
}

//work from here.
private[spec] case class MultiRelationType(types: List[RelationTypeOrInstance], fingerprint: UUID = UUID.randomUUID())
    extends CypherType(typeOf[List[RelationTypeOrInstance]], fingerprint) {

  def or[U <: Product, UH <: HList](rel: U, properties: UH)(
      implicit queryProvider: QueryProvider[U],
      i1: ToTraversable.Aux[UH, List, Symbol]): MultiRelationType = {
    val relationTypeOrInstance = RelationTypeOrInstance(Relationship(rel, properties))
    copy(types :+ relationTypeOrInstance)
  }
  def or(rel: RelationType): MultiRelationType = {
    val relationTypeOrInstance = RelationTypeOrInstance(rel)
    copy(types :+ relationTypeOrInstance)
  }

  override def toQuery(context: Context): DSLResult = {
    context
      .map(fingerprint) { result =>
        DSLResult(s"[${result.trim}]")
      }
      .getOrElse {
        val id = context.add(fingerprint)
        val result = types.map(_.toQuery(context)).foldLeft(List.empty[String], Map.empty[String, Any]) {
          (acc, result) =>
            (acc._1 :+ result.query, acc._2 ++ result.queryMap)
        }
        DSLResult(s"[$id:${result._1.mkString("|:")}]", result._2)
      }
  }

}

private[cypherDSL] case class RelationType(tpe: Type,
                                           variableLengthRelation: Option[VariableLengthRelation] = None,
                                           fingerprint: UUID = UUID.randomUUID())
    extends CypherType(tpe, fingerprint)
    with SnakeCasing {

  override def toQuery(context: Context): DSLResult = {
    val varLengthStringIfAny = variableLengthRelation.map(_.toQuery(context)).mkString
    val result               = super.toQuery(context)
    result.copy(query = s"[${result.query}$varLengthStringIfAny]", queryMap = result.queryMap)
  }

  def or[U <: Product, UH <: HList](rel: U, properties: UH)(implicit queryProvider: QueryProvider[U],
                                                            i1: ToTraversable.Aux[UH, List, Symbol]): MultiRelationType = {
    val relationTypeOrInstance = RelationTypeOrInstance(Relationship(rel, properties))
    MultiRelationType(List(RelationTypeOrInstance(tpe), relationTypeOrInstance))
  }

  def or(rel: RelationType): MultiRelationType = {
    val relationTypeOrInstance = RelationTypeOrInstance(rel)
    MultiRelationType(List(RelationTypeOrInstance(tpe), relationTypeOrInstance))
  }
  override def label: String = upperSnakeCased(super.label)
}

object RelationType {
  def apply(tpe: Type): RelationType = new RelationType(tpe, None)
  def apply(tpe: Type, variableLengthRelation: VariableLengthRelation): RelationType =
    new RelationType(tpe, Option(variableLengthRelation))
}
