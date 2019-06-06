package com.agrim.scala.cypherDSL.spec.entities
import com.agrim.scala.cypherDSL.spec.Context

private[cypherDSL] trait CypherEntity {
  def toQuery(context: Context = new Context()): String
}
