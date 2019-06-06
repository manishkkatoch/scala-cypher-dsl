package com.agrim.scala.cypherDSL.spec.operators
import com.agrim.scala.cypherDSL.spec.Context

private[spec] trait Operator {
  def toQuery(context: Context): String
}
