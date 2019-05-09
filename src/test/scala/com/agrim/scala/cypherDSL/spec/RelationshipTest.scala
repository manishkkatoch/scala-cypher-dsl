package com.agrim.scala.cypherDSL.spec

import org.scalatest.{Matchers, WordSpec}
import shapeless.HNil
import com.agrim.scala.cypherDSL.spec.utils.Random.randomize
import com.agrim.scala.cypherDSL.spec.utils.TestClasses.Person

class RelationshipTest extends WordSpec with Matchers {

  private val person = randomize[Person]
  "Relationship" must {
    ".toQuery" must {
      "given a Product not in Context" must {
        "provide a string representation with default properties" in {
          val context    = new Context()
          val personNode = Relationship(person, HNil)
          personNode.toQuery(context) shouldBe "[a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}}]"
        }
        "provide a string representation with some properties" in {
          val context    = new Context()
          val personNode = Relationship(person, 'id :: 'age :: HNil)
          personNode.toQuery(context) shouldBe "[a0:Person {id: {a0_id},age: {a0_age}}]"
        }
      }
      "given a Product in Context" must {
        val context = new Context()
        context.add(person)

        "provide a string representation when no properties provided" in {
          val personNode = Relationship(person, HNil)
          personNode.toQuery(context) shouldBe "[a0]"
        }
        "provide a string representation when some properties provided" in {
          val personNode = Relationship(person, 'age :: HNil)
          personNode.toQuery(context) shouldBe "[a0]"
        }
      }
    }
  }
}
