package com.agrim.scala.cypherDSL.spec

import org.scalatest.{Matchers, WordSpec}
import shapeless.HNil
import utils.Random.randomize
import utils.TestClasses.IsFriendOf

class RelationshipTest extends WordSpec with Matchers {
  private val isFriendOf = randomize[IsFriendOf]

  "Relationship" should {
    ".toQuery" when {
      "Product is not in the context" must {
        "return a string representation with default properties" in {
          implicit val context = new Context
          val relationship     = Relationship(isFriendOf, HNil)

          relationship.toQuery shouldBe "[a0:IS_FRIEND_OF {since: {a0_since},lastConnectedOn: {a0_lastConnectedOn}}]"
        }

        "return a string representation with some properties" in {
          implicit val context = new Context
          val relationship     = Relationship(isFriendOf, 'since :: HNil)

          relationship.toQuery shouldBe "[a0:IS_FRIEND_OF {since: {a0_since}}]"
        }
      }

      "Product is in the Context" must {
        "return a string representation when no properties provided" in {
          implicit val context = new Context()
          context.add(isFriendOf)

          val relationship = Relationship(isFriendOf, HNil)

          relationship.toQuery shouldBe "[a0]"
        }

        "return a string representation when some properties provided" in {
          implicit val context = new Context()
          context.add(isFriendOf)

          val relationship = Relationship(isFriendOf, 'since :: HNil)

          relationship.toQuery shouldBe "[a0]"
        }
      }
    }
  }
}
