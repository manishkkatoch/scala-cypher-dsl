package me.manishkatoch.scala.cypherDSL.spec.clauses
import org.scalatest.{Matchers, WordSpec}

class LimitsTest extends WordSpec with Matchers {
  "Limits" should {
    "provide query for a given count" in {
      Limits(10).toQuery() shouldBe "LIMIT 10"
    }
  }
}
