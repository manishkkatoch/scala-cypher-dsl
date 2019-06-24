package me.manishkatoch.scala.cypherDSL.spec.entities

import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult}

private[cypherDSL] trait CypherEntity {
  def toQuery(context: Context = new Context()): DSLResult
}
