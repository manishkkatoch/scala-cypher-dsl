package me.manishkatoch.scala.cypherDSL.spec

case class DSLResult(query: String, queryMap: Map[String,Any])

object DSLResult {
  def apply(query: String):DSLResult = DSLResult(query, Map.empty)
}