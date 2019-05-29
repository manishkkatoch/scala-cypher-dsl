package com.agrim.scala.cypherDSL.spec

sealed trait Operator {
  def toQuery(context: Context): String
}

case class DistinctOperator(element: AliasedProduct) extends Operator with ElementPropertyExtractingAndAliasing {
  override def toQuery(context: Context): String = {
    val (el, properties) = getElementAndProperties(element.node)
    val ids = context
      .get(el)
      .map(identifier => {
        if (element.alias.isDefined) context.update(element.node, element.alias.get)
        makeAliasedString(identifier, properties, element.alias)
      })
      .getOrElse(throw new NoSuchElementException("DISTINCT operator requires element to be in Context"))

    if (ids.nonEmpty) s"DISTINCT $ids" else ""
  }
}
