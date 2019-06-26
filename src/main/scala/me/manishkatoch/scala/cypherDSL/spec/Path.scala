package me.manishkatoch.scala.cypherDSL.spec

import me.manishkatoch.scala.cypherDSL.spec.entities._
import me.manishkatoch.scala.cypherDSL.spec.utils.SnakeCasing
import shapeless.ops.hlist.ToTraversable
import shapeless.{HList, HNil}

import scala.util.Try

private[cypherDSL] class Path(val pathLinks: PathLink*) {
  def toQuery(context: Context = new Context()): DSLResult = {
    val (queryList, paramMap) =
      pathLinks.map(_.toQuery(context)).foldLeft((List.empty[String], Map.empty[String, Any])) { (acc, result) =>
        (acc._1 :+ result.query, acc._2 ++ result.queryMap)
      }
    DSLResult(queryList.mkString, paramMap)
  }

  def |[T <: Product, TH <: HList, U <: Product, UH <: HList](rel: U)(
      implicit queryProvider: QueryProvider[U]): Path = {
    val lastLink = pathLinks.last
    val newRelElement = Try(Left(lastLink.element.asInstanceOf[Relationship[T, TH]]))
      .recover { case _ => Right(lastLink.element.asInstanceOf[RelationType]) }
      .get
      .fold(_.or(rel, HNil), _.or(rel, HNil))
    new Path(pathLinks.take(pathLinks.length - 1) :+ PathLink(lastLink.leftLink, newRelElement, lastLink.rightLink): _*)
  }
  def |[T <: Product, TH <: HList](rel: RelationType): Path = {
    val lastLink = pathLinks.last
    val newRelElement = Try(Left(lastLink.element.asInstanceOf[Relationship[T, TH]]))
      .recover { case _ => Right(lastLink.element.asInstanceOf[RelationType]) }
      .get
      .fold(_.or(rel), _.or(rel))
    new Path(pathLinks.take(pathLinks.length - 1) :+ PathLink(lastLink.leftLink, newRelElement, lastLink.rightLink): _*)
  }

  def |-[U <: Product, UH <: HList](rel: U)(implicit qpu: QueryProvider[U]): Path = {
    new Path(pathLinks :+ PathLink(Some("-"), Node(rel, HNil), None): _*)
  }

  def |-[U <: Product, UH <: HList](node: Node[U, UH])(implicit qpu: QueryProvider[U]): Path = {
    new Path(pathLinks :+ PathLink(Some("-"), node, None): _*)
  }

  def |-[U <: Product, UH <: HList](rel: NodeType): Path = {
    new Path(pathLinks :+ PathLink(Some("-"), rel, None): _*)
  }

  def |-[U <: Product, UH <: HList](rel: RelationType): Path = {
    new Path(pathLinks :+ PathLink(Some("-"), rel, None): _*)
  }
  def |-[U <: Product, UH <: HList](path: Path): Path = {
    val lastLink = pathLinks.last.copy(rightLink = Option("-"))
    new Path(
      (pathLinks.take(pathLinks.length - 1) :+ lastLink) ++
        path.pathLinks: _*)
  }

  def |->[U <: Product, UH <: HList](rel: U)(implicit qpu: QueryProvider[U]): Path = {
    new Path(pathLinks :+ PathLink(Some("->"), Node(rel, HNil), None): _*)
  }

  def |->[U <: Product, UH <: HList](node: Node[U, UH])(implicit qpu: QueryProvider[U]): Path = {
    new Path(pathLinks :+ PathLink(Some("->"), node, None): _*)
  }

  def |->(node: NodeType): Path = {
    new Path(pathLinks :+ PathLink(Some("->"), node, None): _*)
  }

  def |->[U <: Product, UH <: HList](path: Path): Path = {
    val lastLink = pathLinks.last.copy(rightLink = Option("->"))
    new Path(
      (pathLinks.take(pathLinks.length - 1) :+ lastLink) ++
        path.pathLinks: _*)
  }
}
