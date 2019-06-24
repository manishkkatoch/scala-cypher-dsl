package me.manishkatoch.scala.cypherDSL.spec

import shapeless.{::, HList, HNil}

private object Utils {

  def matchPropertyPattern(identifier: String, propertyName: String) =
    s"$propertyName: {${propertyValuePlaceHolder(identifier,propertyName)}}"

  def propertyValuePlaceHolder(identifier: String, propertyName: String) = s"${identifier}_$propertyName"

  def toList[H <: HList](list: H): List[String] = {
    def lpToList(list: HList): List[String] = list match {
      case HNil                => List.empty
      case (s: Symbol) :: tail => s.name +: lpToList(tail)
      case s :: tail           => s.toString +: lpToList(tail)
    }
    lpToList(list)
  }
  def toAnyList[H <: HList](list: H): List[Any] = {
    def lpToList(list: HList): List[Any] = list match {
      case HNil                => List.empty
      case (s: Symbol) :: tail => s.name +: lpToList(tail)
      case s :: tail           => s +: lpToList(tail)
    }
    lpToList(list)
  }
}
