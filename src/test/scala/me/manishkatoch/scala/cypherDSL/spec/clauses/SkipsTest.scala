package me.manishkatoch.scala.cypherDSL.spec.clauses
import org.scalatest.{Matchers, WordSpec}

class SkipsTest extends WordSpec with Matchers {

  "Skips" should {
    "provide query for a given count" in {
      Skips(10).toQuery() shouldBe "SKIP 10"
    }
  }

}
