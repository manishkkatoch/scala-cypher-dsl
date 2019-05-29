package com.agrim.scala.cypherDSL.spec.entities

import com.agrim.scala.cypherDSL.spec.{Context, UpperSnakeCasing}

import scala.reflect.runtime.universe.Type

private[spec] sealed abstract class CypherType(tpe: Type) extends CypherEntity {
  def toQuery(context: Context = new Context()): String = {
    context.map(this) { _.trim }.getOrElse {
      val id = context.add(this)
      s"$id:$label"
    }
  }

  def label: String = tpe.typeSymbol.asClass.name.decodedName.toString
}

private[cypherDSL] case class NodeType(tpe: Type, alias: Option[String] = None) extends CypherType(tpe) {
  override def toQuery(context: Context): String = s"(${super.toQuery(context)})"
}

private[cypherDSL] case class RelationType(tpe: Type, alias: Option[String] = None)
    extends CypherType(tpe)
    with UpperSnakeCasing {
  override def toQuery(context: Context): String = s"[${super.toQuery(context)}]"

  override def label: String = upperSnakeCased(super.label)
}
