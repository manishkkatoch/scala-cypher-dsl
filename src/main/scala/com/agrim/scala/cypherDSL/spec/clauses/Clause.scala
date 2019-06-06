package com.agrim.scala.cypherDSL.spec.clauses
import com.agrim.scala.cypherDSL.spec.Context

private[spec] trait Clause {
  def toQuery(context: Context): String
}
