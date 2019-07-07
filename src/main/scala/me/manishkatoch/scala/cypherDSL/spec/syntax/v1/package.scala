package me.manishkatoch.scala.cypherDSL.spec.syntax

import me.manishkatoch.scala.cypherDSL.spec.clauses._
import me.manishkatoch.scala.cypherDSL.spec.entities.{CypherEntity, Node, NodeType, RelationType, Relationship}
import me.manishkatoch.scala.cypherDSL.spec.operators.Distinct
import me.manishkatoch.scala.cypherDSL.spec.{Path, QueryProvider, Statement}
import shapeless.HList
import shapeless.ops.hlist.ToTraversable

import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeOf}

package object v1 {
  def cypher                            = Statement()
  def any[T <: Product: WeakTypeTag]    = NodeType(weakTypeOf[T])
  def anyNode                           = NodeType(weakTypeOf[Any])
  def anyRel[T <: Product: WeakTypeTag] = RelationType(weakTypeOf[T])
  def anyRelation                       = RelationType(weakTypeOf[Any])

  implicit class MatcherStatement(statement: Statement) {
    def MATCH[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ Matches(element))
    }
    def MATCH(element: NodeType): Statement = {
      statement.copy(clauses = statement.clauses :+ Matches(element))
    }
    def MATCH[T <: Product, TH <: HList](element: Node[T, TH])(implicit queryProvider: QueryProvider[T],
                                                               i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ Matches(element))
    }

    def MATCH(element: Path): Statement = {
      statement.copy(clauses = statement.clauses :+ Matches(element))
    }
    def OPTIONAL_MATCH[T <: Product](element: NodeType): Statement = {
      statement.copy(clauses = statement.clauses :+ OptionallyMatches(element))
    }
    def OPTIONAL_MATCH[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ OptionallyMatches(element))
    }
    def OPTIONAL_MATCH[T <: Product, TH <: HList](element: Node[T, TH])(
        implicit queryProvider: QueryProvider[T],
        i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ OptionallyMatches(element))
    }
    def OPTIONAL_MATCH(element: Path): Statement = {
      statement.copy(clauses = statement.clauses :+ OptionallyMatches(element))
    }

    def CREATE[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ Creates(element))
    }
    def CREATE(element: NodeType): Statement = {
      statement.copy(clauses = statement.clauses :+ Creates(element))
    }
    def CREATE[T <: Product, TH <: HList](element: Node[T, TH])(implicit queryProvider: QueryProvider[T],
                                                                i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ Creates(element))
    }

    def CREATE(element: Path): Statement = {
      statement.copy(clauses = statement.clauses :+ Creates(element))
    }

    def MERGE[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ Merges(element))
    }
    def MERGE(element: NodeType): Statement = {
      statement.copy(clauses = statement.clauses :+ Merges(element))
    }
    def MERGE[T <: Product, TH <: HList](element: Node[T, TH])(implicit queryProvider: QueryProvider[T],
                                                               i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ Merges(element))
    }

    def MERGE(element: Path): Statement = {
      statement.copy(clauses = statement.clauses :+ Merges(element))
    }

    def DELETE[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = false))
    }
    def DELETE(element: NodeType): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = false))
    }
    def DELETE[T <: Product, TH <: HList](element: Node[T, TH])(implicit queryProvider: QueryProvider[T],
                                                                i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = false))
    }

    def DELETE(element: Path): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = false))
    }

    def DETACH_DELETE[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = true))
    }
    def DETACH_DELETE(element: NodeType): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = true))
    }
    def DETACH_DELETE[T <: Product, TH <: HList](element: Node[T, TH])(
        implicit queryProvider: QueryProvider[T],
        i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = true))
    }

    def DETACH_DELETE(element: Path): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = true))
    }

    def SET[T <: Product, TH <: HList](element: T, setters: List[(CypherEntity, Any)])(
        implicit queryProvider: QueryProvider[T],
        i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ Sets(element, setters))
    }

    def SET[T <: Product](setters: (Node[T, _], Any)*)(
      implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ Sets(setters:_*))
    }


    def SKIP(count: Int): Statement = {
      statement.copy(clauses = statement.clauses :+ Skips(count))
    }

    def LIMIT(count: Int): Statement = {
      statement.copy(clauses = statement.clauses :+ Limits(count))
    }

    def RETURN(elements: Product*): Statement = {
      statement.copy(clauses = statement.clauses :+ Returns(elements: _*))
    }

    def WITH[T <: Product](elements: T*): Statement = {
      statement.copy(clauses = statement.clauses :+ With(elements: _*))
    }


    def ORDER_BY[T <: Product](elements: T*): Statement = {
      statement.copy(clauses = statement.clauses :+ OrdersBy(elements: _*))
    }
    def ORDER_BY_DESC[T <: Product](elements: T*): Statement = {
      statement.copy(clauses = statement.clauses :+ OrdersBy(true, elements: _*))
    }
  }
}
