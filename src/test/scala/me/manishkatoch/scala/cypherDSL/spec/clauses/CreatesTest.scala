package me.manishkatoch.scala.cypherDSL.spec.clauses

import me.manishkatoch.scala.cypherDSL.spec.DSLResult
import me.manishkatoch.scala.cypherDSL.spec.syntax.any
import me.manishkatoch.scala.cypherDSL.spec.syntax.patterns._
import me.manishkatoch.scala.cypherDSL.spec.utils.Random.randomize
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.{Department, Person}
import org.scalatest.{Matchers, WordSpec}

class CreatesTest extends WordSpec with Matchers {
  "Creates" should {
    val person: Person         = randomize[Person]
    val department: Department = randomize[Department]

    "provide query for a single case class" in {
      val creates = Creates(person)
      creates.toQuery() shouldBe DSLResult("CREATE (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})",
        Map("a0_id" -> person.id, "a0_name" -> person.name, "a0_age" -> person.age))
    }
    "provide query for a node instance" in {
      val creates = Creates(person('name))
      creates.toQuery() shouldBe DSLResult("CREATE (a0:Person {name: {a0_name}})", Map("a0_name" -> person.name))
    }
    "provide query for a any node" in {
      val creates = Creates(any[Person])
      creates.toQuery() shouldBe DSLResult("CREATE (a0:Person)")
    }
    "provide query for a path" in {
      val creates = Creates(person --> department)
      creates.toQuery() shouldBe DSLResult(
        "CREATE (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-->(a1:Department {id: {a1_id},name: {a1_name}})",
        Map("a0_id"   -> person.id,
          "a0_name" -> person.name,
          "a0_age"  -> person.age,
          "a1_id"   -> department.id,
          "a1_name" -> department.name)
      )
    }

  }

}
