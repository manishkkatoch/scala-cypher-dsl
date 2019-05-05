package com.agrim.scala.cypherDSL.spec.implicits

import shapeless.{HList, LabelledGeneric, Witness}
import shapeless.ops.record.Selector

private[spec] trait PropertyExists[T, PName, PType]
private[spec] object PropertyExists {
  def apply[T, PType](column: Witness)(
      implicit exists: PropertyExists[T, column.T, PType]): PropertyExists[T, column.T, PType] =
    exists

  implicit def implicitProvider[T, H <: HList, PName, PType](
      implicit
      gen: LabelledGeneric.Aux[T, H],
      selector: Selector.Aux[H, PName, PType]
  ): PropertyExists[T, PName, PType] = new PropertyExists[T, PName, PType] {}
}
