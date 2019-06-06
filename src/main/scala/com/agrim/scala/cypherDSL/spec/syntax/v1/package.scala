package com.agrim.scala.cypherDSL.spec.syntax

import com.agrim.scala.cypherDSL.spec.clauses._
import com.agrim.scala.cypherDSL.spec.entities.{Node, NodeType, RelationType}
import com.agrim.scala.cypherDSL.spec.{Path, QueryProvider, Statement}
import shapeless.HList
import shapeless.ops.hlist.ToTraversable

import scala.reflect.runtime.universe.{weakTypeOf, WeakTypeTag}

package object v1 {
  def cypher                            = Statement()
  def any[T <: Product: WeakTypeTag]    = NodeType(weakTypeOf[T])
  def anyRel[T <: Product: WeakTypeTag] = RelationType(weakTypeOf[T])

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
