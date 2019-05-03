package com.agrim.scala.cypherDSL.spec

private[spec] sealed trait Clause {
  def toQuery(implicit context: Context): String
}

private[spec] class Returns(elements: Product*) extends Clause {

  private val errorMessage = "One or more of the elements to be returned are not in Context!"

  @throws[NoSuchElementException]
  def toQuery(implicit context: Context): String = {
    val ids = elements
      .map(
        element =>
          context
            .get(element)
            .getOrElse(throw new NoSuchElementException(errorMessage)))
      .mkString(",")

    if (ids.nonEmpty) s"RETURN $ids" else ""
  }
}

private[spec] object Returns {
  def apply(elements: Product*): Returns = new Returns(elements: _*)
  val empty                              = Returns(Seq.empty: _*)
}
