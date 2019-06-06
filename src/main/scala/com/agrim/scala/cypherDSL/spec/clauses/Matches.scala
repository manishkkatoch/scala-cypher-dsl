package com.agrim.scala.cypherDSL.spec.clauses
import com.agrim.scala.cypherDSL.spec._
import com.agrim.scala.cypherDSL.spec.entities.{Node, NodeType}
import shapeless.ops.hlist.ToTraversable
import shapeless.{HList, HNil}

private[spec] class Matches(path: Path) extends Clause {
  override def toQuery(context: Context = new Context()): String = s"MATCH ${path.toQuery(context)}"
}

private[spec] object Matches {
  def apply[T <: Product, TH <: HList](element: Node[T, TH])(
      implicit i0: ToTraversable.Aux[TH, List, Symbol]): Matches = {
    val path = new Path(PathLink(None, element, None))
    new Matches(path)
  }
  def apply(element: NodeType): Matches = {
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
