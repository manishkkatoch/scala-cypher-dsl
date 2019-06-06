package com.agrim.scala.cypherDSL.spec.clauses
import com.agrim.scala.cypherDSL.spec.Context

private[spec] class Skips(count: Int) extends Clause {
  override def toQuery(context: Context = new Context()): String = s"SKIP $count"
}
private[spec] object Skips {
  def apply(count: Int) = new Skips(count)
}
