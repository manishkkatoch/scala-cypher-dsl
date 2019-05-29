package com.agrim.scala.cypherDSL.spec

import com.agrim.scala.cypherDSL.spec.utils.Random.randomize
import com.agrim.scala.cypherDSL.spec.utils.TestClasses.ImplicitCache._
import com.agrim.scala.cypherDSL.spec.utils.TestClasses.{Department, Person, WorksIn}
import org.scalatest.{Matchers, WordSpec}

class OptionallyMatchesTest extends WordSpec with Matchers {
  private val person: Person         = randomize[Person]
  private val department: Department = randomize[Department]
  private val worksIn: WorksIn       = randomize[WorksIn]

  "OptionallyMatches" should {
    "provide OPTIONAL MATCH for a single product" in {
      implicit val context: Context = new Context()

      val matches = OptionallyMatches(person)
      matches.toQuery(context) shouldBe "OPTIONAL MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})"
    }
    "provide OPTIONAL MATCH for a single product in context" in {
      implicit val context: Context = new Context()
      context.add(person)

      val matches = OptionallyMatches(person)
      matches.toQuery(context) shouldBe "OPTIONAL MATCH (a0)"
    }
    "provide OPTIONAL MATCH for a path" in {
      implicit val context: Context = new Context()

      val matches = new OptionallyMatches(person -| worksIn |-> department)
      matches.toQuery(context) shouldBe "OPTIONAL MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})"

    }
    "provide OPTIONAL MATCH for a path in context" in {
      implicit val context: Context = new Context()
      context.add(person)
      context.add(worksIn)
      context.add(department)

      val matches = new OptionallyMatches(person -| worksIn |-> department)
      matches.toQuery(context) shouldBe "OPTIONAL MATCH (a0)-[a1]->(a2)"
    }
  }
}
