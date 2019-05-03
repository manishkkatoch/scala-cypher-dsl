package utils

import com.agrim.scala.cypherDSL.spec.Context

object TestClasses {
  case class Person(id: String, name: String, age: Int)
  case class Department(id: String, name: String)

  object implicits {
    implicit val context = new Context()
//    implicit val personQP = QueryProvider[Person]
//    implicit val deptQP   = QueryProvider[Department]

  }
}
