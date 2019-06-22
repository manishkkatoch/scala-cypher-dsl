package me.manishkatoch.scala.cypherDSL.spec.operators

import me.manishkatoch.scala.cypherDSL.spec.Context

private[spec] trait Operator {
  def toQuery(context: Context): String
}
