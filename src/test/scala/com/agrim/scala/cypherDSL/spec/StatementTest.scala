package com.agrim.scala.cypherDSL.spec

import org.scalatest.{Matchers, WordSpec}
import com.agrim.scala.cypherDSL.spec.utils.Random._
import com.agrim.scala.cypherDSL.spec.utils.TestClasses.Person

class StatementTest extends WordSpec with Matchers {

  private val person           = randomize[Person]
  private implicit val context = new Context()

  "A Statement" must {
    context.add(person)

    "return RETURN string if passed a return clause" in {
      Statement(Seq(Returns(person))).toQuery(context) shouldBe "RETURN a0"
    }

    "return empty string if passed no clauses" in {
      Statement().toQuery(context) shouldBe ""
    }
  }

}
