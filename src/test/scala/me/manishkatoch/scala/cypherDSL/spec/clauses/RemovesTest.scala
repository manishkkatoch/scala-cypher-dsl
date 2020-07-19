package me.manishkatoch.scala.cypherDSL.spec.clauses

import me.manishkatoch.scala.cypherDSL.spec.syntax.patterns._
import me.manishkatoch.scala.cypherDSL.spec.utils.Random.randomize
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.ImplicitCache._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.Person
import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult}
import org.scalatest.{Matchers, WordSpec}

class RemovesTest extends WordSpec with Matchers {

  "Removes" should {
    val person: Person         = randomize[Person]
    "remove property from a node" in {
      val context = new Context()
      context.add(person)
      val removes = Removes(person('name))
      removes.toQuery(context) shouldBe DSLResult("REMOVE a0.name");
    }

    "remove multiple properties from a node" in {
      val context = new Context()
      context.add(person)
      val removes = Removes(person('name, 'id))
      removes.toQuery(context) shouldBe DSLResult("REMOVE a0.name,a0.id");
    }
  }
}
