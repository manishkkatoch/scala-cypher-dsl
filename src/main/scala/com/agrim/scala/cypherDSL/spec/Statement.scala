package com.agrim.scala.cypherDSL.spec

private[cypherDSL] case class Statement(clauses: Seq[Clause]) {
  def toQuery(context: Context): String = {
    clauses
      .map(clause => clause.toQuery(context))
      .mkString(System.lineSeparator())
  }
}

private[cypherDSL] object Statement {
  def apply(): Statement = new Statement(Seq.empty)
}
