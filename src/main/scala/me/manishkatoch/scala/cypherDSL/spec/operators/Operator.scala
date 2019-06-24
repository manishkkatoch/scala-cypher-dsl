package me.manishkatoch.scala.cypherDSL.spec.operators

import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult}

private[spec] trait Operator {
  def toQuery(context: Context): DSLResult
}
