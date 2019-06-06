package me.manishkatoch.scala.cypherDSL.spec.utils

import me.manishkatoch.scala.cypherDSL.spec.QueryProvider

object TestClasses {
  case class Person(id: String, name: String, age: Int)
  case class IsFriendOf(since: Int, lastConnectedOn: String)
  case class Department(id: String, name: String)
  case class LocatedIn(area: String)
  case class Region(name: String)
  case class HeadOfDepartment(id: String, name: String)
  case class TenPropClass(prop1: String,
                          prop2: String,
                          prop3: Int,
                          prop4: Int,
                          prop5: String,
                          prop6: String,
                          prop7: String,
                          prop8: Int,
                          prop9: Int,
                          prop10: String)
  case class WorksIn(sinceDays: Int)

  object ImplicitCache {
    private def aQueryProvider[T <: Product](implicit queryProvider: QueryProvider[T]) = queryProvider

    implicit val personQp           = aQueryProvider[Person]
    implicit val departmentQp       = aQueryProvider[Department]
    implicit val worksInQp          = aQueryProvider[WorksIn]
    implicit val locatedInQp        = aQueryProvider[LocatedIn]
    implicit val regionQp           = aQueryProvider[Region]
    implicit val headOfDepartmentQp = aQueryProvider[HeadOfDepartment]

  }
}
