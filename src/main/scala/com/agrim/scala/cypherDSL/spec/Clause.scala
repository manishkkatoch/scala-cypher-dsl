package com.agrim.scala.cypherDSL.spec

import com.agrim.scala.cypherDSL.spec.implicits.QueryProvider
import shapeless.{HList, HNil}
import shapeless.ops.hlist.ToTraversable

private[cypherDSL] sealed trait Clause {
  def toQuery(context: Context): String
}

private[cypherDSL] class Matches(path: Path) extends Clause {
  override def toQuery(context: Context = new Context()): String = s"MATCH ${path.toQuery(context)}"
}
private[cypherDSL] object Matches {
  def apply[T <: Product, TH <: HList](element: Node[T, TH])(
      implicit i0: ToTraversable.Aux[TH, List, Symbol]): Matches = {
    val path = new Path(PathLink(None, element, None))
    new Matches(path)
  }
  def apply[T <: Product, TH <: HList](element: T)(implicit queryProvider: QueryProvider[T],
                                                   i0: ToTraversable.Aux[TH, List, Symbol]): Matches = {
    val path = new Path(PathLink(None, Node(element, HNil), None))
    new Matches(path)
  }
  def apply(path: Path) = new Matches(path)
}

private[cypherDSL] class OptionallyMatches(path: Path) extends Clause {
  override def toQuery(context: Context = new Context()): String = s"OPTIONAL MATCH ${path.toQuery(context)}"
}
private[cypherDSL] object OptionallyMatches {
  def apply[T <: Product, TH <: HList](element: Node[T, TH])(
      implicit i0: ToTraversable.Aux[TH, List, Symbol]): OptionallyMatches = {
    val path = new Path(PathLink(None, element, None))
    new OptionallyMatches(path)
  }
  def apply[T <: Product, TH <: HList](element: T)(implicit queryProvider: QueryProvider[T],
                                                   i0: ToTraversable.Aux[TH, List, Symbol]): OptionallyMatches = {
    val path = new Path(PathLink(None, Node(element, HNil), None))
    new OptionallyMatches(path)
  }
  def apply(path: Path) = new OptionallyMatches(path)
}

private case class ReturnAliasing(node: Product, alias: Option[String])
private object ReturnAliasing {
  def apply(product: Product): ReturnAliasing = ReturnAliasing(product, None)
}

private[cypherDSL] class Returns(elements: ReturnAliasing*) extends Clause {

  private val errorMessage = "One or more of the elements to be returned are not in Context!"

  @throws[NoSuchElementException]
  def toQuery(context: Context = new Context()): String = {
    val ids = elements
      .map(
        element =>
          context
            .get(element.node)
            .map(_ + element.alias.map(alias => s" as $alias").mkString)
            .getOrElse(throw new NoSuchElementException(errorMessage)))
      .mkString(",")

    if (ids.nonEmpty) s"RETURN $ids" else ""
  }
}

private[cypherDSL] object Returns {

  private def makeReturnAliasing(list: List[Product]): Seq[ReturnAliasing] = list match {
    case Nil                                 => List.empty
    case (s: (Product, String)) :: remaining => ReturnAliasing(s._1, Option(s._2)) +: makeReturnAliasing(remaining)
    case (s: Product) :: remaining           => ReturnAliasing(s, None) +: makeReturnAliasing(remaining)
  }

  def apply(elements: Product*): Returns = new Returns(makeReturnAliasing(elements.toList): _*)
  val empty                              = Returns(Seq.empty: _*)
}
