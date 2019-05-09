package com.agrim.scala.cypherDSL.spec

import com.agrim.scala.cypherDSL.spec.implicits.QueryProvider
import shapeless.{HList, HNil}

private[cypherDSL] class Path(val pathLinks: PathLink[_, _ <: HList]*) {
  def toQuery(context: Context = new Context()): String =
    pathLinks.map(_.toQuery(context)).mkString

  def <-|[U <: Product, UH <: HList](rel: U)(implicit queryProvider: QueryProvider[U]): Path = {
    new Path(pathLinks :+ PathLink(Some("<-"), Relationship(rel, HNil), None): _*)
  }

  def <-|[U <: Product, UH <: HList](path: Path): Path = {
    val lastLink = pathLinks.last.asInstanceOf[PathLink[U, UH]].copy(rightLink = Option("<-"))
    new Path(
      (pathLinks.take(pathLinks.length - 1) :+ lastLink) ++
        path.pathLinks: _*)
  }

  def -|[U <: Product, UH <: HList](rel: U)(implicit queryProvider: QueryProvider[U]): Path = {
    new Path(pathLinks :+ PathLink(Some("-"), Node(rel, HNil), None): _*)
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

  def -|[U <: Product, UH <: HList](path: Path): Path = {
    val lastLink = pathLinks.last.asInstanceOf[PathLink[U, UH]].copy(rightLink = Option("-"))
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
