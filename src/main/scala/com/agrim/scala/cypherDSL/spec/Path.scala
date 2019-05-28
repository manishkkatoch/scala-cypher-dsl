package com.agrim.scala.cypherDSL.spec

import shapeless.{HList, HNil}

private[cypherDSL] class Path(val pathLinks: PathLink[_, _ <: HList]*) {
  def toQuery(context: Context = new Context()): String =
    pathLinks.map(_.toQuery(context)).mkString

  def |[T <: Product, TH <: HList, U <: Product, UH <: HList](rel: U)(
      implicit queryProvider: QueryProvider[U]): Path = {
    val lastLink      = pathLinks.last.asInstanceOf[PathLink[U, UH]]
    val newRelElement = lastLink.element.asInstanceOf[Relationship[T, TH]].or(rel, HNil)
    new Path(pathLinks.take(pathLinks.length - 1) :+ PathLink(lastLink.leftLink, newRelElement, lastLink.rightLink): _*)
  }

  def |->[U <: Product, UH <: HList](rel: U)(implicit queryProvider: QueryProvider[U]): Path = {
    new Path(pathLinks :+ PathLink(Some("->"), Node(rel, HNil), None): _*)
  }

  def |->[U <: Product, UH <: HList](path: Path): Path = {
    val lastLink = pathLinks.last.asInstanceOf[PathLink[U, UH]].copy(rightLink = Option("->"))
    new Path(
      (pathLinks.take(pathLinks.length - 1) :+ lastLink) ++
        path.pathLinks: _*)
  }

  def |-[U <: Product, UH <: HList](path: Path): Path = {
    val lastLink = pathLinks.last.asInstanceOf[PathLink[U, UH]].copy(rightLink = Option("-"))
    new Path(
      (pathLinks.take(pathLinks.length - 1) :+ lastLink) ++
        path.pathLinks: _*)
  }

  def |-[U <: Product, UH <: HList](rel: U)(implicit qpu: QueryProvider[U]): Path = {
    new Path(pathLinks :+ PathLink(Some("-"), Node(rel, HNil), None): _*)
  }
}
