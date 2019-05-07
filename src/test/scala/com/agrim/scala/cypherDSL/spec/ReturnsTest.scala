package com.agrim.scala.cypherDSL.spec

import org.scalatest.{Matchers, WordSpec}
import com.agrim.scala.cypherDSL.spec.utils.Random._
import com.agrim.scala.cypherDSL.spec.utils.TestClasses.{Department, Person}

class ReturnsTest extends WordSpec with Matchers {

  private val personA          = randomize[Person]
  private val personB          = randomize[Person]
  private val departmentA      = randomize[Department]
  private implicit val context = new Context()

  "Returns" should {
    context.add(personA)
    context.add(departmentA)

    "return query for an element in Context" in {
      Returns(personA).toQuery shouldBe "RETURN a0"
    }

    "return empty statement if no elements passed" in {
      Returns().toQuery shouldBe ""
    }

    "return query for more than one element in Context" in {
      Returns(personA, departmentA).toQuery shouldBe "RETURN a0,a1"
    }

    "throw if element to be returned not in Context" in {
      the[NoSuchElementException] thrownBy {
        Returns(personB).toQuery
      } should have message "One or more of the elements to be returned are not in Context!"
    }
  }
}
