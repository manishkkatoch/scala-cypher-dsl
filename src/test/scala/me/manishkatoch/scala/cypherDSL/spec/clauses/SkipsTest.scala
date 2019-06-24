package me.manishkatoch.scala.cypherDSL.spec.clauses

import me.manishkatoch.scala.cypherDSL.spec.DSLResult
import org.scalatest.{Matchers, WordSpec}

class SkipsTest extends WordSpec with Matchers {

  "Skips" should {
    "provide query for a given count" in {
      Skips(10).toQuery() shouldBe DSLResult("SKIP 10")
    }
  }

}
