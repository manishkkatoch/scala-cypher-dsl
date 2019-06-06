package me.manishkatoch.scala.cypherDSL.spec

import org.scalatest.{Matchers, WordSpec}
import me.manishkatoch.scala.cypherDSL.spec.utils.Random._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.Person

class StatementTest extends WordSpec with Matchers {

  private val person           = randomize[Person]
  private implicit val context = new Context()

  "A Statement" must {
    context.add(person)

    "return empty string if passed no clauses" in {
      Statement().toQuery(context) shouldBe ""
    }
  }

}
