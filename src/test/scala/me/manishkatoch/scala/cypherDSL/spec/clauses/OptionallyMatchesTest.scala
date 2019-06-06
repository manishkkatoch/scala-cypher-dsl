package me.manishkatoch.scala.cypherDSL.spec.clauses
import me.manishkatoch.scala.cypherDSL.spec.syntax.any
import me.manishkatoch.scala.cypherDSL.spec.syntax.patterns._
import me.manishkatoch.scala.cypherDSL.spec.utils.Random.randomize
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.ImplicitCache._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.{Department, Person}
import org.scalatest.{Matchers, WordSpec}

class OptionallyMatchesTest extends WordSpec with Matchers {
  "Matches" should {
    val person: Person         = randomize[Person]
    val department: Department = randomize[Department]

    "provide query for a single case class" in {
      val matches = OptionallyMatches(person)
      matches.toQuery() shouldBe "OPTIONAL MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})"
    }
    "provide query for a node instance" in {
      val matches = OptionallyMatches(person('name))
      matches.toQuery() shouldBe "OPTIONAL MATCH (a0:Person {name: {a0_name}})"
    }
    "provide query for a any node" in {
      val matches = OptionallyMatches(any[Person])
      matches.toQuery() shouldBe "OPTIONAL MATCH (a0:Person)"

    }
    "provide query for a path" in {
      val matches = OptionallyMatches(person --> department)
      matches.toQuery() shouldBe "OPTIONAL MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-->(a1:Department {id: {a1_id},name: {a1_name}})"

    }

  }
}
