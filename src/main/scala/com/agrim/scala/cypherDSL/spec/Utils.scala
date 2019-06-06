package com.agrim.scala.cypherDSL.spec

import shapeless.{::, HList, HNil}

private object Utils {
  def matchPropertyPattern(identifier: String, propertyName: String) = s"$propertyName: {${identifier}_$propertyName}"

  def toList[H <: HList](list: H): List[String] = {
    def lpToList(list: HList): List[String] = list match {
      case HNil                => List.empty
      case (s: Symbol) :: tail => s.name +: lpToList(tail)
      case s :: tail           => s.toString +: lpToList(tail)
    }
    lpToList(list)
  }

}
