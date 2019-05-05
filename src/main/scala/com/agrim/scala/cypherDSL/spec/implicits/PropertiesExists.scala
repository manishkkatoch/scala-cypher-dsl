package com.agrim.scala.cypherDSL.spec.implicits

import shapeless.{::, HList, HNil}

private[spec] trait PropertiesExists[T, PName <: HList, PType]

private[spec] object PropertiesExists {
  implicit def forHNil[T, PName, PType](
      implicit head: PropertyExists[T, PName, PType]): PropertiesExists[T, PName :: HNil, PType] =
    new PropertiesExists[T, PName :: HNil, PType] {}

  implicit def forHList[T, PNameHead, PNameTail <: HList, PTypeForHead, PTypeForTail](
      implicit headExists: PropertyExists[T, PNameHead, PTypeForHead],
      tailExists: PropertiesExists[T, PNameTail, PTypeForTail])
    : PropertiesExists[T, PNameHead :: PNameTail, PTypeForTail] =
    new PropertiesExists[T, PNameHead :: PNameTail, PTypeForTail] {}
}
