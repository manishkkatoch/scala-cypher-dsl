package me.manishkatoch.scala.cypherDSL.spec.clauses

import me.manishkatoch.scala.cypherDSL.spec.DSLResult
import me.manishkatoch.scala.cypherDSL.spec.syntax._
import me.manishkatoch.scala.cypherDSL.spec.syntax.patterns._
import me.manishkatoch.scala.cypherDSL.spec.utils.Random._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.ImplicitCache._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.{Department, Person}
import org.scalatest.{Matchers, WordSpec}

class MatchesTest extends WordSpec with Matchers {
  "Matches" should {
    val person: Person         = randomize[Person]
    val department: Department = randomize[Department]

    "provide query for a single case class" in {
      val matches = Matches(person)
      matches.toQuery() shouldBe DSLResult("MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})",
                                           Map("a0_id" -> person.id, "a0_name" -> person.name, "a0_age" -> person.age))
    }
    "provide query for a node instance" in {
      val matches = Matches(person('name))
      matches.toQuery() shouldBe DSLResult("MATCH (a0:Person {name: {a0_name}})", Map("a0_name" -> person.name))
    }
    "provide query for a any node" in {
      val matches = Matches(any[Person])
      matches.toQuery() shouldBe DSLResult("MATCH (a0:Person)")
    }
    "provide query for a path" in {
      val matches = Matches(person --> department)
      matches.toQuery() shouldBe DSLResult(
        "MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-->(a1:Department {id: {a1_id},name: {a1_name}})",
        Map("a0_id"   -> person.id,
            "a0_name" -> person.name,
            "a0_age"  -> person.age,
            "a1_id"   -> department.id,
            "a1_name" -> department.name)
      )
    }

  }
}
