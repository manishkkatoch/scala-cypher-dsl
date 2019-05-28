package com.agrim.scala.cypherDSL.spec

import shapeless.ops.hlist.ToTraversable
import shapeless.{HList, HNil}
import Utils._

import scala.util.Try

private[cypherDSL] sealed trait Clause {
  def toQuery(context: Context): String
}

private[cypherDSL] class Skips(count: Int) extends Clause {
  override def toQuery(context: Context): String = s"SKIP $count"
}

private[cypherDSL] object Skips {
  def apply(count: Int) = new Skips(count)
}

private[cypherDSL] class Limits(count: Int) extends Clause {
  override def toQuery(context: Context): String = s"LIMIT $count"
}

private[cypherDSL] object Limits {
  def apply(count: Int) = new Limits(count)
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

private[cypherDSL] class Returns(elements: ReturnAliasing*) extends Clause {

  private val errorMessage                    = "One or more of the elements to be returned are not in Context!"
  private val tooManyPropertiesToAliasMessage = "Alias one property at a time!"

  @throws[NoSuchElementException]
  def toQuery(context: Context = new Context()): String = {
    val ids = elements
      .map(element => {
        val (el, properties) = getElementAndProperties(element)
        context
          .get(el)
          .map(identifier => makeAliasedReturnString(identifier, properties, element.alias))
          .getOrElse(throw new NoSuchElementException(errorMessage))
      })
      .mkString(",")

    if (ids.nonEmpty) s"RETURN $ids" else ""
  }

  private def getElementAndProperties(returnAliasing: ReturnAliasing): (Product, List[String]) =
    returnAliasing.node match {
      case s: Node[_, _] => (s.element, toList(s.properties))
      case s             => (s, List.empty[String])
    }

  @throws[AssertionError]
  private def makeAliasedReturnString(identifier: String, properties: List[String], alias: Option[String]) = {
    if (properties.length > 1 && alias.isDefined) {
      throw new AssertionError(tooManyPropertiesToAliasMessage)
    }
    val identifierString = properties.map(p => s"$identifier.$p").mkString(",")
    val aliasString      = alias.map(a => s" as $a").mkString
    (if (identifierString.isEmpty) identifier else identifierString) + aliasString
  }
}

private[cypherDSL] object Returns {

  private def makeReturnAliasing(list: List[Product]): Seq[ReturnAliasing] = list match {
    case Nil                                 => List.empty
    case (s: (Product, String)) :: remaining => ReturnAliasing(s._1, Option(s._2)) +: makeReturnAliasing(remaining)
    case (s: Product) :: remaining           => ReturnAliasing(s, None) +: makeReturnAliasing(remaining)
  }

  def apply(elements: Product*): Returns = new Returns(makeReturnAliasing(elements.toList): _*)

  val empty = Returns(Seq.empty: _*)
}

private case class OrderingProduct(element: Product)

private[cypherDSL] class OrdersBy(descendingOrder: Boolean, elements: OrderingProduct*) extends Clause {
  private val errorMessage = "One or more of the elements to be returned are not in Context!"

  private def makeAliasedReturnString(identifier: String, properties: List[String]): String = {
    val identifierString = properties.map(p => s"$identifier.$p").mkString(",")
    if (identifierString.isEmpty) identifier else identifierString
  }

  override def toQuery(context: Context): String = {
    val ids = elements
      .map(element => {
        val (el, properties) = getElementAndProperties(element)
        context
          .get(el)
          .map(identifier => makeAliasedReturnString(identifier, properties))
          .getOrElse(throw new NoSuchElementException(errorMessage))
      })
      .mkString(",")
    (if (ids.nonEmpty) s"ORDER BY $ids $getOrderingString" else "").trim
  }

  private def getOrderingString = if (descendingOrder) "DESC" else ""

  private def getElementAndProperties(orderingProduct: OrderingProduct): (Product, List[String]) =
    orderingProduct.element match {
      case s: Node[_, _] => (s.element, toList(s.properties))
      case s             => (s, List.empty[String])
    }
}

private[cypherDSL] object OrdersBy {
  private def makeOrderingProduct(list: List[Product]): Seq[OrderingProduct] = list match {
    case Nil                       => List.empty
    case (s: Product) :: remaining => OrderingProduct(s) +: makeOrderingProduct(remaining)
  }

  def apply(elements: Product*): OrdersBy = new OrdersBy(false, makeOrderingProduct(elements.toList): _*)

  def apply(descendingOrder: Boolean, elements: Product*): OrdersBy =
    new OrdersBy(descendingOrder, makeOrderingProduct(elements.toList): _*)

  val empty = OrdersBy(Seq.empty: _*)
}
