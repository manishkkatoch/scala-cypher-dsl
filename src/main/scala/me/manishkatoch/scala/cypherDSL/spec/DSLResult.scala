package me.manishkatoch.scala.cypherDSL.spec

case class DSLResult(query: String, queryMap: Map[String, Any]) {
  def ++(result: DSLResult, separator: String = ""): DSLResult =
    copy(query + separator + result.query, queryMap ++ result.queryMap)

  def isEmpty:Boolean = query == "" && queryMap.isEmpty
}

object DSLResult {
  def apply(query: String): DSLResult = DSLResult(query, Map.empty)
  val empty: DSLResult = DSLResult("")
}
