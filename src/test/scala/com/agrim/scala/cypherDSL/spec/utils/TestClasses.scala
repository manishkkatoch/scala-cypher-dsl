package com.agrim.scala.cypherDSL.spec.utils

import com.agrim.scala.cypherDSL.spec.implicits.QueryProvider

object TestClasses {
  case class Person(id: String, name: String, age: Int)
  case class Department(id: String, name: String)
  case class LocatedIn()
  case class Region(name: String)
  case class HeadOfDepartment(id: String, name: String)
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
