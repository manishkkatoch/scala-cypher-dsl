package me.manishkatoch.scala.cypherDSL.spec

import me.manishkatoch.scala.cypherDSL.spec.clauses.Clause

private[cypherDSL] case class Statement(clauses: Seq[Clause]) {
  def toQuery(context: Context = new Context()): DSLResult = {
    val (queryList, paramMap) = clauses.map(clause => clause.toQuery(context))
    .foldLeft((List.empty[String], Map.empty[String,Any])) {(acc, result) =>
      (acc._1 :+ result.query, acc._2 ++ result.queryMap)
    }
    DSLResult(queryList.mkString(System.lineSeparator()), paramMap)
  }
}

private[cypherDSL] object Statement {
  def apply(): Statement = new Statement(Seq.empty)
}
