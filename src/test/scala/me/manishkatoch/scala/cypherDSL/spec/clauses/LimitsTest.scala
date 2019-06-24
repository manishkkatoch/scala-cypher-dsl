package me.manishkatoch.scala.cypherDSL.spec.clauses

import me.manishkatoch.scala.cypherDSL.spec.DSLResult
import org.scalatest.{Matchers, WordSpec}

class LimitsTest extends WordSpec with Matchers {
  "Limits" should {
    "provide query for a given count" in {
      Limits(10).toQuery() shouldBe DSLResult("LIMIT 10")
    }
  }
}
