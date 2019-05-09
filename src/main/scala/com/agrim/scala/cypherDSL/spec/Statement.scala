package com.agrim.scala.cypherDSL.spec

private[cypherDSL] case class Statement(readingClause: Seq[Clause], returnClause: Returns) {
  def toQuery(context: Context): String = {
    (readingClause.map(clause => clause.toQuery(context)) :+ returnClause.toQuery(context))
      .mkString(System.lineSeparator())
  }
  def addReadingClause(clause: Clause): Statement = copy(readingClause = readingClause :+ clause)
  def addReturnClause(clause: Returns): Statement = copy(returnClause = clause)

}

private[cypherDSL] object Statement {
  def apply(): Statement = new Statement(Seq.empty, Returns.empty)
}
