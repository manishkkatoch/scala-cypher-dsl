package me.manishkatoch.scala.cypherDSL.spec.entities

import me.manishkatoch.scala.cypherDSL.spec.Context

private[cypherDSL] case class VariableLengthRelation(startLength: Option[Int] = None, endLength: Option[Int] = None) {
  def toQuery(context: Context = new Context()): String = {
    val rangeString = Seq(startLength.map(_.toString).getOrElse(""), endLength.map(_.toString).getOrElse(""))
      .filter(_.nonEmpty)
      .mkString("..")
    s"*$rangeString"
  }
}
private[cypherDSL] object VariableLengthRelation {
  def apply(startLength: Int, endLength: Int): VariableLengthRelation =
    VariableLengthRelation(Option(startLength), Option(endLength))
  def apply(startLength: Int): VariableLengthRelation =
    VariableLengthRelation(Option(startLength), None)
  def apply(): VariableLengthRelation =
    VariableLengthRelation(None, None)
}
