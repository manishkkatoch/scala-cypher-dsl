package me.manishkatoch.scala.cypherDSL.spec.clauses

import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult}

private[cypherDSL] class Limits(count: Int) extends Clause {
  override def toQuery(context: Context = new Context()): DSLResult = DSLResult(s"LIMIT $count")
}
private[cypherDSL] object Limits {
  def apply(count: Int) = new Limits(count)
}
