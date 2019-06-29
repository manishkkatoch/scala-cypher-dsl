package me.manishkatoch.scala.cypherDSL.spec.clauses

import me.manishkatoch.scala.cypherDSL.spec.entities.{Node, NodeType}
import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult, Path, PathLink, QueryProvider}
import shapeless.{HList, HNil}
import shapeless.ops.hlist.ToTraversable

private[spec] class Deletes(path: Path, shouldDetach: Boolean) extends Clause {
  override def toQuery(context: Context = new Context()): DSLResult = {
    val result = path.toQuery(context)
    val command = s"${if(shouldDetach) "DETACH " else "" }DELETE"
    result.copy(query = s"$command ${result.query}")
  }
}

private[spec] object Deletes {
  def apply[T <: Product, TH <: HList](element: Node[T, TH], detaches: Boolean)(
    implicit i0: ToTraversable.Aux[TH, List, Symbol]): Deletes = {
    val path = new Path(PathLink(None, element, None))
    new Deletes(path,detaches)
  }
  def apply(element: NodeType, detaches: Boolean): Deletes = {
    val path = new Path(PathLink(None, element, None))
    new Deletes(path,detaches)
  }

  def apply[T <: Product, TH <: HList](element: T, detaches: Boolean)(implicit queryProvider: QueryProvider[T],
                                                   i0: ToTraversable.Aux[TH, List, Symbol]): Deletes = {
    val path = new Path(PathLink(None, Node(element, HNil), None))
    new Deletes(path,detaches)
  }

  def apply(path: Path, detaches: Boolean) = new Deletes(path, detaches)
}
