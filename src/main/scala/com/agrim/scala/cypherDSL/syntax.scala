package com.agrim.scala.cypherDSL

import com.agrim.scala.cypherDSL.spec.{Path, PathLink}
import com.agrim.scala.cypherDSL.spec.implicits.QueryProvider
import com.agrim.scala.cypherDSL.spec.{Context, Node, Relationship}
import shapeless.ops.hlist.ToTraversable
import shapeless.{HList, HNil}

object syntax {

  implicit class RichProduct[T <: Product, TH <: HList](element: T) {
    def --[U <: Product, UH <: HList](rel: U)(implicit context: Context, qpT: QueryProvider[T], qpU: QueryProvider[U]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(Some("-"), Node(rel, HNil), None))

    def -->[U <: Product, UH <: HList](
        rel: U)(implicit context: Context, qpU: QueryProvider[U], qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(Some("->"), Node(rel, HNil), None))

    def <--[U <: Product, UH <: HList](
        rel: U)(implicit context: Context, qpU: QueryProvider[U], qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("<-")), PathLink(Some("-"), Node(rel, HNil), None))

    def <-|[U <: Product, UH <: HList](
        rel: U)(implicit context: Context, qpU: QueryProvider[U], qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("<-")), PathLink(None, Relationship(rel, HNil), None))

    def -|(rel: Path)(implicit context: Context, qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")))

    def -|[U <: Product, UH <: HList](rel: U)(implicit context: Context, qpU: QueryProvider[U], qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, Relationship(rel, HNil), None))
  }

  implicit class RichNode[T <: Product](element: T) {
    def apply(implicit context: Context, queryProvider: QueryProvider[T]) = Node(element, HNil)

    def apply(p1: Symbol)(implicit context: Context, queryProvider: QueryProvider[T]) = Node(element, p1 :: HNil)

    def apply(p1: Symbol, p2: Symbol)(implicit context: Context, queryProvider: QueryProvider[T]) =
      Node(element, p1 :: p2 :: HNil)

    def apply(p1: Symbol, p2: Symbol, p3: Symbol)(implicit context: Context, queryProvider: QueryProvider[T]) =
      Node(element, p1 :: p2 :: p3 :: HNil)

    def apply(p1: Symbol, p2: Symbol, p3: Symbol, p4: Symbol)(implicit context: Context,
                                                              queryProvider: QueryProvider[T]) =
      Node(element, p1 :: p2 :: p3 :: p4 :: HNil)

    def apply(p1: Symbol, p2: Symbol, p3: Symbol, p4: Symbol, p5: Symbol)(implicit context: Context,
                                                                          queryProvider: QueryProvider[T]) =
      Node(element, p1 :: p2 :: p3 :: p4 :: p5 :: HNil)

    def apply(p1: Symbol, p2: Symbol, p3: Symbol, p4: Symbol, p5: Symbol, p6: Symbol)(implicit context: Context,
                                                                                      queryProvider: QueryProvider[T]) =
      Node(element, p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: HNil)

    def apply(p1: Symbol, p2: Symbol, p3: Symbol, p4: Symbol, p5: Symbol, p6: Symbol, p7: Symbol)(
        implicit context: Context,
        queryProvider: QueryProvider[T]) =
      Node(element, p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: HNil)

    def apply(p1: Symbol, p2: Symbol, p3: Symbol, p4: Symbol, p5: Symbol, p6: Symbol, p7: Symbol, p8: Symbol)(
        implicit context: Context,
        queryProvider: QueryProvider[T]) =
      Node(element, p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: HNil)

    def apply(p1: Symbol,
              p2: Symbol,
              p3: Symbol,
              p4: Symbol,
              p5: Symbol,
              p6: Symbol,
              p7: Symbol,
              p8: Symbol,
              p9: Symbol)(implicit context: Context, queryProvider: QueryProvider[T]) =
      Node(element, p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: HNil)

    def apply(p1: Symbol,
              p2: Symbol,
              p3: Symbol,
              p4: Symbol,
              p5: Symbol,
              p6: Symbol,
              p7: Symbol,
              p8: Symbol,
              p9: Symbol,
              p10: Symbol)(implicit context: Context, queryProvider: QueryProvider[T]) =
      Node(element, p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: HNil)
  }

  implicit class EnrichedRichNode[T <: Product, TH <: HList](element: Node[T, TH]) {
    def <-|[U <: Product, UH <: HList](rel: U)(implicit context: Context, queryProvider: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(None, Relationship(rel, HNil), None))

    def <-|[U <: Product, UH <: HList](rel: Node[U, UH])(implicit context: Context,
                                                         queryProvider: QueryProvider[U],
                                                         i0: ToTraversable.Aux[UH, List, Symbol]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(None, Relationship(rel.element, rel.properties), None))

    def -|[U <: Product, UH <: HList](rel: U)(implicit context: Context, queryProvider: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("-")), PathLink(None, Relationship(rel, HNil), None))

    def --[U <: Product, UH <: HList](rel: U)(implicit context: Context, queryProvider: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("-"), Node(rel, HNil), None))

    def --[U <: Product, UH <: HList](rel: Node[U, UH])(implicit context: Context) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("-"), rel, None))

    def -->[U <: Product, UH <: HList](rel: U)(implicit context: Context, queryProvider: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("->"), Node(rel, HNil), None))

    def -->[U <: Product, UH <: HList](rel: Node[U, UH])(implicit context: Context) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("->"), rel, None))

    def <--[U <: Product, UH <: HList](rel: U)(implicit context: Context, queryProvider: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(Some("-"), Node(rel, HNil), None))

    def <--[U <: Product, UH <: HList](rel: Node[U, UH])(implicit context: Context) =
      new Path(PathLink(None, element, Some("<-")), PathLink(Some("-"), rel, None))
  }

}
