package com.agrim.scala.cypherDSL.spec

import com.agrim.scala.cypherDSL.spec.utils.Random._
import com.agrim.scala.cypherDSL.spec.utils.TestClasses.{Department, Person, WorksIn}
import com.agrim.scala.cypherDSL.spec.utils.TestClasses.ImplicitCache._
import org.scalatest.{Matchers, WordSpec}
import com.agrim.scala.cypherDSL.syntax._

class MatchesTest extends WordSpec with Matchers {
  private val person: Person         = randomize[Person]
  private val department: Department = randomize[Department]
  private val worksIn: WorksIn       = randomize[WorksIn]

  "Matches" should {
    "provide MATCH for a single product" in {
      implicit val context: Context = new Context()

      val matches = Matches(person)
      matches.toQuery shouldBe "MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})"
    }
    "provide MATCH for a single product in context" in {
      implicit val context: Context = new Context()
      context.add(person)

      val matches = Matches(person)
      matches.toQuery shouldBe "MATCH (a0)"
    }
    "provide MATCH for a path" in {
      implicit val context: Context = new Context()

      val matches = new Matches(person -| worksIn |-> department)
      matches.toQuery shouldBe "MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WorksIn {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})"
    }
    "provide MATCH for a path in context" in {
      implicit val context: Context = new Context()
      context.add(person)
      context.add(worksIn)
      context.add(department)

      val matches = new Matches(person -| worksIn |-> department)
      matches.toQuery shouldBe "MATCH (a0)-[a1]->(a2)"
    }
  }
}
