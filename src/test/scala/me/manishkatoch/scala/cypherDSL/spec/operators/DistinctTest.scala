package me.manishkatoch.scala.cypherDSL.spec.operators

import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult}
import me.manishkatoch.scala.cypherDSL.spec.entities.AliasedProduct
import me.manishkatoch.scala.cypherDSL.spec.syntax.patterns._
import me.manishkatoch.scala.cypherDSL.spec.utils.Random.randomize
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.ImplicitCache._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.Person
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}

class DistinctTest extends WordSpec with Matchers with BeforeAndAfterEach {
  private var context: Context = _
  private var person: Person   = _

  "DISTINCT" should {
    "provide query for an aliased case class if no alias provided in context" in {
      Distinct(person).toQuery(context) shouldBe DSLResult("DISTINCT a0")
    }
    "provide query for an aliased case class if alias provided in context" in {
      Distinct(person -> "worker").toQuery(context) shouldBe DSLResult("DISTINCT a0 as worker")
    }
    "provide query for an aliased node if no alias provided in context" in {
      Distinct(person('name)).toQuery(context) shouldBe DSLResult("DISTINCT a0.name")
    }
    "provide query for an aliased node if alias provided in context" in {
      Distinct(person('name) -> "workerName")
        .toQuery(context) shouldBe DSLResult("DISTINCT a0.name as workerName")
    }
  }

  override def beforeEach() {
    context = new Context()
    person = randomize[Person]
    context.add(person)
  }

}
