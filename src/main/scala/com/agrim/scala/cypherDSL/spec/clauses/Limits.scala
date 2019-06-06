package com.agrim.scala.cypherDSL.spec.clauses
import com.agrim.scala.cypherDSL.spec.Context

private[cypherDSL] class Limits(count: Int) extends Clause {
  override def toQuery(context: Context = new Context()): String = s"LIMIT $count"
}
private[cypherDSL] object Limits {
  def apply(count: Int) = new Limits(count)
}
