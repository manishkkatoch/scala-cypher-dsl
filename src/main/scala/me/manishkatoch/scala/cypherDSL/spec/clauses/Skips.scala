package me.manishkatoch.scala.cypherDSL.spec.clauses

import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult}

private[spec] class Skips(count: Int) extends Clause {
  override def toQuery(context: Context = new Context()): DSLResult = DSLResult(s"SKIP $count")
}
private[spec] object Skips {
  def apply(count: Int) = new Skips(count)
}
