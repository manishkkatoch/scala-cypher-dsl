package com.agrim.scala.cypherDSL.spec

private[cypherDSL] case class Statement(returnClause: Returns) {
  def toQuery(implicit context: Context): String = {
    returnClause.toQuery
  }

  def addReturnClause(clause: Returns): Statement = copy(returnClause = clause)
}

private[cypherDSL] object Statement {
  def apply(): Statement = new Statement(Returns.empty)
}
