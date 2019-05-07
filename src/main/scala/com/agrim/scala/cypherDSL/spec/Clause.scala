package com.agrim.scala.cypherDSL.spec

import com.agrim.scala.cypherDSL.spec.implicits.QueryProvider
import shapeless.{HList, HNil}
import shapeless.ops.hlist.ToTraversable

private[spec] sealed trait Clause {
  def toQuery(implicit context: Context): String
}

private[spec] class Matches(path: Path) extends Clause {
  override def toQuery(implicit context: Context): String = s"MATCH ${path.toQuery}"
}
private[spec] object Matches {
  def apply[T <: Product, TH <: HList](element: T)(implicit context: Context,
                                                   queryProvider: QueryProvider[T],
                                                   i0: ToTraversable.Aux[TH, List, Symbol]): Matches = {
    val path = new Path(PathLink(None, Node(element, HNil), None))
    new Matches(path)
  }
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
