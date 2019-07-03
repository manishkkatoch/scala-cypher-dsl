package me.manishkatoch.scala.cypherDSL.spec.syntax

import me.manishkatoch.scala.cypherDSL.spec.entities._
import me.manishkatoch.scala.cypherDSL.spec.{Path, PathLink, QueryProvider}
import shapeless.ops.hlist.ToTraversable
import shapeless.ops.record.Selector
import shapeless.{HList, HNil, LabelledGeneric, Witness}

import scala.annotation.implicitNotFound

private[spec] object Patterns {

  @implicitNotFound(msg = "${PName} not found in ${T}")
  private trait PropertyExists[T, PName, PType]

  private object PropertyExists {
    def apply[T, PType](column: Witness)(
        implicit exists: PropertyExists[T, column.T, PType]): PropertyExists[T, column.T, PType] =
      exists

    implicit def implicitProvider[T, H <: HList, PName, PType](
        implicit
        gen: LabelledGeneric.Aux[T, H],
        selector: Selector.Aux[H, PName, PType]
    ): PropertyExists[T, PName, PType] = new PropertyExists[T, PName, PType] {}
  }

  implicit class RichNode[T <: Product](element: T) {

    def apply[TP](p1: Witness.Lt[Symbol])(implicit queryProvider: QueryProvider[T],
                                          sp1: PropertyExists[T, p1.T, TP]) = {
      Node(element, p1.value :: HNil)
    }

    def apply[TP, TP2](p1: Witness.Lt[Symbol], p2: Witness.Lt[Symbol])(implicit queryProvider: QueryProvider[T],
                                                                       sp1: PropertyExists[T, p1.T, TP],
                                                                       sp2: PropertyExists[T, p2.T, TP2]) = {
      Node(element, p1.value :: p2.value :: HNil)
    }

    def apply[TP, TP2, TP3](p1: Witness.Lt[Symbol], p2: Witness.Lt[Symbol], p3: Witness.Lt[Symbol])(
        implicit queryProvider: QueryProvider[T],
        sp1: PropertyExists[T, p1.T, TP],
        sp2: PropertyExists[T, p2.T, TP2],
        sp3: PropertyExists[T, p3.T, TP3]) = {
      Node(element, p1.value :: p2.value :: p3.value :: HNil)
    }

    def apply[TP, TP2, TP3, TP4](p1: Witness.Lt[Symbol],
                                 p2: Witness.Lt[Symbol],
                                 p3: Witness.Lt[Symbol],
                                 p4: Witness.Lt[Symbol])(implicit queryProvider: QueryProvider[T],
                                                         sp1: PropertyExists[T, p1.T, TP],
                                                         sp2: PropertyExists[T, p2.T, TP2],
                                                         sp3: PropertyExists[T, p3.T, TP3],
                                                         sp4: PropertyExists[T, p4.T, TP4]) = {
      Node(element, p1.value :: p2.value :: p3.value :: p4.value :: HNil)
    }

    def apply[TP, TP2, TP3, TP4, TP5](p1: Witness.Lt[Symbol],
                                      p2: Witness.Lt[Symbol],
                                      p3: Witness.Lt[Symbol],
                                      p4: Witness.Lt[Symbol],
                                      p5: Witness.Lt[Symbol])(implicit queryProvider: QueryProvider[T],
                                                              sp1: PropertyExists[T, p1.T, TP],
                                                              sp2: PropertyExists[T, p2.T, TP2],
                                                              sp3: PropertyExists[T, p3.T, TP3],
                                                              sp4: PropertyExists[T, p4.T, TP4],
                                                              sp5: PropertyExists[T, p5.T, TP5]) = {
      Node(element, p1.value :: p2.value :: p3.value :: p4.value :: p5.value :: HNil)
    }

    def apply[TP, TP2, TP3, TP4, TP5, TP6](p1: Witness.Lt[Symbol],
                                           p2: Witness.Lt[Symbol],
                                           p3: Witness.Lt[Symbol],
                                           p4: Witness.Lt[Symbol],
                                           p5: Witness.Lt[Symbol],
                                           p6: Witness.Lt[Symbol])(implicit queryProvider: QueryProvider[T],
                                                                   sp1: PropertyExists[T, p1.T, TP],
                                                                   sp2: PropertyExists[T, p2.T, TP2],
                                                                   sp3: PropertyExists[T, p3.T, TP3],
                                                                   sp4: PropertyExists[T, p4.T, TP4],
                                                                   sp5: PropertyExists[T, p5.T, TP5],
                                                                   sp6: PropertyExists[T, p6.T, TP6]) = {
      Node(element, p1.value :: p2.value :: p3.value :: p4.value :: p5.value :: p6.value :: HNil)
    }

    def apply[TP, TP2, TP3, TP4, TP5, TP6, TP7](p1: Witness.Lt[Symbol],
                                                p2: Witness.Lt[Symbol],
                                                p3: Witness.Lt[Symbol],
                                                p4: Witness.Lt[Symbol],
                                                p5: Witness.Lt[Symbol],
                                                p6: Witness.Lt[Symbol],
                                                p7: Witness.Lt[Symbol])(implicit queryProvider: QueryProvider[T],
                                                                        sp1: PropertyExists[T, p1.T, TP],
                                                                        sp2: PropertyExists[T, p2.T, TP2],
                                                                        sp3: PropertyExists[T, p3.T, TP3],
                                                                        sp4: PropertyExists[T, p4.T, TP4],
                                                                        sp5: PropertyExists[T, p5.T, TP5],
                                                                        sp6: PropertyExists[T, p6.T, TP6],
                                                                        sp7: PropertyExists[T, p7.T, TP7]) = {
      Node(element, p1.value :: p2.value :: p3.value :: p4.value :: p5.value :: p6.value :: p7.value :: HNil)
    }

    def apply[TP, TP2, TP3, TP4, TP5, TP6, TP7, TP8](p1: Witness.Lt[Symbol],
                                                     p2: Witness.Lt[Symbol],
                                                     p3: Witness.Lt[Symbol],
                                                     p4: Witness.Lt[Symbol],
                                                     p5: Witness.Lt[Symbol],
                                                     p6: Witness.Lt[Symbol],
                                                     p7: Witness.Lt[Symbol],
                                                     p8: Witness.Lt[Symbol])(implicit queryProvider: QueryProvider[T],
                                                                             sp1: PropertyExists[T, p1.T, TP],
                                                                             sp2: PropertyExists[T, p2.T, TP2],
                                                                             sp3: PropertyExists[T, p3.T, TP3],
                                                                             sp4: PropertyExists[T, p4.T, TP4],
                                                                             sp5: PropertyExists[T, p5.T, TP5],
                                                                             sp6: PropertyExists[T, p6.T, TP6],
                                                                             sp7: PropertyExists[T, p7.T, TP7],
                                                                             sp8: PropertyExists[T, p8.T, TP8]) = {
      Node(element,
           p1.value :: p2.value :: p3.value :: p4.value :: p5.value :: p6.value :: p7.value :: p8.value :: HNil)
    }

    def apply[TP, TP2, TP3, TP4, TP5, TP6, TP7, TP8, TP9](p1: Witness.Lt[Symbol],
                                                          p2: Witness.Lt[Symbol],
                                                          p3: Witness.Lt[Symbol],
                                                          p4: Witness.Lt[Symbol],
                                                          p5: Witness.Lt[Symbol],
                                                          p6: Witness.Lt[Symbol],
                                                          p7: Witness.Lt[Symbol],
                                                          p8: Witness.Lt[Symbol],
                                                          p9: Witness.Lt[Symbol])(
        implicit queryProvider: QueryProvider[T],
        sp1: PropertyExists[T, p1.T, TP],
        sp2: PropertyExists[T, p2.T, TP2],
        sp3: PropertyExists[T, p3.T, TP3],
        sp4: PropertyExists[T, p4.T, TP4],
        sp5: PropertyExists[T, p5.T, TP5],
        sp6: PropertyExists[T, p6.T, TP6],
        sp7: PropertyExists[T, p7.T, TP7],
        sp8: PropertyExists[T, p8.T, TP8],
        sp9: PropertyExists[T, p9.T, TP9]) = {
      Node(
        element,
        p1.value :: p2.value :: p3.value :: p4.value :: p5.value :: p6.value :: p7.value :: p8.value :: p9.value :: HNil)
    }

    def apply[TP, TP2, TP3, TP4, TP5, TP6, TP7, TP8, TP9, TP10](p1: Witness.Lt[Symbol],
                                                                p2: Witness.Lt[Symbol],
                                                                p3: Witness.Lt[Symbol],
                                                                p4: Witness.Lt[Symbol],
                                                                p5: Witness.Lt[Symbol],
                                                                p6: Witness.Lt[Symbol],
                                                                p7: Witness.Lt[Symbol],
                                                                p8: Witness.Lt[Symbol],
                                                                p9: Witness.Lt[Symbol],
                                                                p10: Witness.Lt[Symbol])(
        implicit queryProvider: QueryProvider[T],
        sp1: PropertyExists[T, p1.T, TP],
        sp2: PropertyExists[T, p2.T, TP2],
        sp3: PropertyExists[T, p3.T, TP3],
        sp4: PropertyExists[T, p4.T, TP4],
        sp5: PropertyExists[T, p5.T, TP5],
        sp6: PropertyExists[T, p6.T, TP6],
        sp7: PropertyExists[T, p7.T, TP7],
        sp8: PropertyExists[T, p8.T, TP8],
        sp9: PropertyExists[T, p9.T, TP9],
        sp10: PropertyExists[T, p10.T, TP9]) = {
      Node(
        element,
        p1.value :: p2.value :: p3.value :: p4.value :: p5.value :: p6.value :: p7.value :: p8.value :: p9.value :: p10.value :: HNil)
    }

  }
  implicit class EnrichedNode[T <: Product, TH <: HList](element: Node[T, TH]) {
    def --[U <: Product, UH <: HList](rel: U)(implicit qpT: QueryProvider[T], qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("-"), Node(rel, HNil), None))

    def --[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("-"), rel, None))

    def --(rel: NodeType)(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("-"), rel, None))

    def -->[U <: Product, UH <: HList](rel: U)(implicit qpT: QueryProvider[T], qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("->"), Node(rel, HNil), None))

    def -->[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("->"), rel, None))

    def -->(rel: NodeType)(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("->"), rel, None))

    def <--[U <: Product, UH <: HList](rel: U)(implicit qpT: QueryProvider[T], qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(Some("-"), Node(rel, HNil), None))

    def <--[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(Some("-"), rel, None))

    def <--(rel: NodeType)(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(Some("-"), rel, None))

    def -|[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) =
      new Path(PathLink(None, element, Some("-")), PathLink(None, Relationship(rel, HNil), None))

    def -|[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                        i0: ToTraversable.Aux[UH, List, Symbol]) = {
      val rels = Relationship(rel.element, rel.properties)
      new Path(PathLink(None, element, Some("-")), PathLink(None, rels, None))
    }

    def -|[U <: Product, UH <: HList](rel: RelationType) =
      new Path(PathLink(None, element, Some("-")), PathLink(None, rel, None))

    def <-|[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(None, Relationship(rel, HNil), None))

    def <-|(rel: RelationType) =
      new Path(PathLink(None, element, Some("<-")), PathLink(None, rel, None))

    def <-|[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                         i0: ToTraversable.Aux[UH, List, Symbol]) = {
      val rels = Relationship(rel.element, rel.properties)
      new Path(PathLink(None, element, Some("<-")), PathLink(None, rels, None))
    }

    def -|*(range: Range)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation(range.start, range.end))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(length: Int)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation(length))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*()(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation())
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) = {
      val variableLengthRelationship = Relationship(rel, HNil, Option(VariableLengthRelation()))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: U, length: Int)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) = {
      val variableLengthRelationship = Relationship(rel, HNil, Option(VariableLengthRelation(length)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: U, range: Range)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) = {
      val variableLengthRelationship = Relationship(rel, HNil, Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                         qpT: QueryProvider[T],
                                                         i1: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship = Relationship(rel.element, rel.properties, Option(VariableLengthRelation()))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH], length: Int)(implicit qpU: QueryProvider[U],
                                                                      qpT: QueryProvider[T],
                                                                      i1: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship = Relationship(rel.element, rel.properties, Option(VariableLengthRelation(length)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH], range: Range)(implicit qpU: QueryProvider[U],
                                                                       qpT: QueryProvider[T],
                                                                       i1: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship =
        Relationship(rel.element, rel.properties, Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
  }
  implicit class RichNodeType(element: NodeType) {
    def --[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("-"), Node(rel, HNil), None))

    def --[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                        i1: ToTraversable.Aux[UH, List, Symbol]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("-"), rel, None))

    def --[U <: Product, UH <: HList](rel: NodeType) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("-"), rel, None))

    def -->[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("->"), Node(rel, HNil), None))

    def -->[U <: Product, UH <: HList](rel: Node[U, UH])(implicit i1: ToTraversable.Aux[UH, List, Symbol]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("->"), rel, None))

    def -->[U <: Product, UH <: HList](rel: NodeType) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("->"), rel, None))

    def <--[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(Some("-"), Node(rel, HNil), None))

    def <--[U <: Product, UH <: HList](rel: Node[U, UH])(implicit i1: ToTraversable.Aux[UH, List, Symbol]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(Some("-"), rel, None))

    def <--[U <: Product, UH <: HList](rel: NodeType) =
      new Path(PathLink(None, element, Some("<-")), PathLink(Some("-"), rel, None))

    def -|[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("-")), PathLink(None, Relationship(rel, HNil), None))

    def -|[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                        i0: ToTraversable.Aux[UH, List, Symbol]) =
      new Path(PathLink(None, element, Some("-")), PathLink(None, Relationship(rel.element, rel.properties), None))

    def -|[U <: Product, UH <: HList](rel: RelationType) =
      new Path(PathLink(None, element, Some("-")), PathLink(None, rel, None))

    def <-|[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(None, Relationship(rel, HNil), None))

    def <-|[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                         i0: ToTraversable.Aux[UH, List, Symbol]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(None, Relationship(rel.element, rel.properties), None))

    def <-|[U <: Product, UH <: HList](rel: RelationType) =
      new Path(PathLink(None, element, Some("<-")), PathLink(None, rel, None))

    def -|*(range: Range) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation(range.start, range.end))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH], range: Range)(implicit qpU: QueryProvider[U],
                                                                       i1: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship =
        Relationship(rel.element, rel.properties, Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product](rel: U, range: Range)(implicit qpU: QueryProvider[U]) = {
      val variableLengthRelationship =
        Relationship(rel, HNil, Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(rel: RelationType, range: Range) = {
      val variableLengthRelationship =
        rel.copy(variableLengthRelation = Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(length: Int) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation(length))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(rel: RelationType, length: Int) = {
      val variableLengthRelationship =
        rel.copy(variableLengthRelation = Option(VariableLengthRelation(length)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(rel: RelationType) = {
      val variableLengthRelationship =
        rel.copy(variableLengthRelation = Option(VariableLengthRelation()))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: U, length: Int)(implicit qpU: QueryProvider[U],
                                                            i0: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship =
        Relationship(rel, HNil, Option(VariableLengthRelation(length)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH], length: Int)(implicit qpU: QueryProvider[U],
                                                                      i0: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship =
        Relationship(rel.element, rel.properties, Option(VariableLengthRelation(length)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*() = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation())
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                         i0: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship =
        Relationship(rel.element, rel.properties, Option(VariableLengthRelation()))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }

    def -|*[U <: Product](rel: U)(implicit qpU: QueryProvider[U]) = {
      val variableLengthRelationship =
        Relationship(rel, HNil, Option(VariableLengthRelation()))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
  }
  implicit class RichProduct[T <: Product, TH <: HList](element: T) {
    def --[U <: Product, UH <: HList](rel: U)(implicit qpT: QueryProvider[T], qpU: QueryProvider[U]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(Some("-"), Node(rel, HNil), None))

    def --[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(Some("-"), rel, None))

    def -->[U <: Product, UH <: HList](rel: U)(implicit qpT: QueryProvider[T], qpU: QueryProvider[U]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(Some("->"), Node(rel, HNil), None))

    def -->[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(Some("->"), rel, None))

    def <--[U <: Product, UH <: HList](rel: U)(implicit qpT: QueryProvider[T], qpU: QueryProvider[U]) =
      new Path(PathLink(None, Node(element, HNil), Some("<-")), PathLink(Some("-"), Node(rel, HNil), None))

    def <--[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("<-")), PathLink(Some("-"), rel, None))

    def <-|[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                         qpT: QueryProvider[T],
                                                         i0: ToTraversable.Aux[UH, List, Symbol]) = {
      val rels = Relationship(rel.element, rel.properties)
      new Path(PathLink(None, Node(element, HNil), Some("<-")), PathLink(None, rels, None))
    }
    def <-|[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("<-")), PathLink(None, Relationship(rel, HNil), None))

    def <-|(rel: RelationType)(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("<-")), PathLink(None, rel, None))

    def -|[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, Relationship(rel, HNil), None))

    def -|(rel: RelationType)(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, rel, None))

    def -|[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                        qpT: QueryProvider[T],
                                                        i0: ToTraversable.Aux[UH, List, Symbol]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")),
               PathLink(None, Relationship(rel.element, rel.properties), None))

    def -|*(range: Range)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation(range.start, range.end))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(length: Int)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation(length))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*()(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation())
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }

    def -|*[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) = {
      val variableLengthRelationship = Relationship(rel, HNil, Option(VariableLengthRelation()))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: U, length: Int)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) = {
      val variableLengthRelationship = Relationship(rel, HNil, Option(VariableLengthRelation(length)))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: U, range: Range)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) = {
      val variableLengthRelationship = Relationship(rel, HNil, Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                         qpT: QueryProvider[T],
                                                         i1: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship = Relationship(rel.element, rel.properties, Option(VariableLengthRelation()))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH], length: Int)(implicit qpU: QueryProvider[U],
                                                                      qpT: QueryProvider[T],
                                                                      i1: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship = Relationship(rel.element, rel.properties, Option(VariableLengthRelation(length)))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH], range: Range)(implicit qpU: QueryProvider[U],
                                                                       qpT: QueryProvider[T],
                                                                       i1: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship =
        Relationship(rel.element, rel.properties, Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(rel: RelationType, range: Range)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship =
        rel.copy(variableLengthRelation = Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(rel: RelationType, length: Int)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship =
        rel.copy(variableLengthRelation = Option(VariableLengthRelation(length)))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(rel: RelationType)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship =
        rel.copy(variableLengthRelation = Option(VariableLengthRelation()))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
  }
}
