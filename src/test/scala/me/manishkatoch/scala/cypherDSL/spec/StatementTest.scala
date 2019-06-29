package me.manishkatoch.scala.cypherDSL.spec

import me.manishkatoch.scala.cypherDSL.spec.clauses.{Matches, Returns}
import org.scalatest.{Matchers, WordSpec}
import me.manishkatoch.scala.cypherDSL.spec.utils.Random._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.Person

class StatementTest extends WordSpec with Matchers {

  private val person = randomize[Person]

  "A Statement" must {

    "return a result for some clauses" in {
      Statement(
        Seq(
          Matches(person),
          Returns(person)
        )).toQuery() shouldBe DSLResult(
        """MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})
          |RETURN a0""".stripMargin,
        Map("a0_id" -> person.id, "a0_name" -> person.name, "a0_age" -> person.age)
      )
    }
    "return empty result if passed no clauses" in {
      Statement().toQuery() shouldBe DSLResult.empty
    }
  }

}
