package me.manishkatoch.scala.cypherDSL.spec.entities

import me.manishkatoch.scala.cypherDSL.spec.Context
import me.manishkatoch.scala.cypherDSL.spec.utils._
import shapeless.HList
import scala.reflect.runtime.universe._

sealed trait QueryableTypeOrInstance

class RelationTypeOrInstance(either: Either[RelationType, Relationship[_, _]]) extends QueryableTypeOrInstance {
  def toQuery(context: Context = new Context()): String = {
    either.fold(_.label, _.toQuery(context).stripSemanticSugar)
  }
}
object RelationTypeOrInstance {
  def apply(tpe: RelationType): RelationTypeOrInstance =
    new RelationTypeOrInstance(Left(tpe))
  def apply(tpe: Type): RelationTypeOrInstance =
    new RelationTypeOrInstance(Left(RelationType(tpe)))
  def apply[T <: Product, TH <: HList](relationship: Relationship[T, TH]): RelationTypeOrInstance =
    new RelationTypeOrInstance(Right(relationship))
}
