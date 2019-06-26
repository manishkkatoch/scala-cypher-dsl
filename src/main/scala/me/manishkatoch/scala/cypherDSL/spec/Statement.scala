package me.manishkatoch.scala.cypherDSL.spec

import me.manishkatoch.scala.cypherDSL.spec.clauses.Clause

/** Statement is the basic building block of Cypher. A Statement contains one or more [[Clause]].
  * The [[Statement]] returns a [[DSLResult]] which is a result of computation of all [[Clause]]s the statement contains
  *
  * @param clauses [[Seq]] of [[Clause]]
  */

private[cypherDSL] case class Statement(private[cypherDSL] val clauses: Seq[Clause]) {
  /**
  * Returns a [[DSLResult]] for this statement
    * @param context any [[Context]] that needs to be reused. Defaults to creating new [[Context]]
    * @return [[DSLResult]]
    */
  def toQuery(context: Context = new Context()): DSLResult = {
    val (queryList, paramMap) = clauses.map(clause => clause.toQuery(context))
    .foldLeft((List.empty[String], Map.empty[String,Any])) {(acc, result) =>
      (acc._1 :+ result.query, acc._2 ++ result.queryMap)
    }
    DSLResult(queryList.mkString(System.lineSeparator()), paramMap)
  }
}

/** Factory for [[me.manishkatoch.scala.cypherDSL.spec.Statement]] instances */
private[cypherDSL] object Statement {

  /** Creates [[Statement]] with no [[Clause]]
    * @return
    */
  def apply(): Statement = new Statement(Seq.empty)
}
