package me.manishkatoch.scala.cypherDSL.spec.clauses

import me.manishkatoch.scala.cypherDSL.spec.Context
import me.manishkatoch.scala.cypherDSL.spec.syntax.any
import me.manishkatoch.scala.cypherDSL.spec.syntax.patterns._
import me.manishkatoch.scala.cypherDSL.spec.utils.Random._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.ImplicitCache._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.{Department, Person}
import org.scalatest.{Matchers, WordSpec}

class ReturnsTest extends WordSpec with Matchers {

  private val personA                   = randomize[Person]
  private val personB                   = randomize[Person]
  private val departmentA               = randomize[Department]
  private val anyPerson                 = any[Person]
  private implicit val context: Context = new Context()

  "Returns" should {
    context.add(personA)
    context.add(departmentA)
    context.add(anyPerson)

    "return query for an element in Context" in {
      Returns(personA).toQuery(context) shouldBe "RETURN a0"
    }
    "return query for any element in Context" in {
      Returns(anyPerson).toQuery(context) shouldBe "RETURN a2"
    }
    "return empty statement if no elements passed" in {
      Returns().toQuery(context) shouldBe ""
    }
    "return query for more than one element in Context" in {
      Returns(personA, departmentA).toQuery(context) shouldBe "RETURN a0,a1"
    }
    "return elements for a property" in {
      Returns(personA('name), departmentA).toQuery(context) shouldBe "RETURN a0.name,a1"
    }
    "return elements for multiple properties" in {
      Returns(personA('name, 'age), departmentA('name)).toQuery(context) shouldBe "RETURN a0.name,a0.age,a1.name"
    }
    "throw if element to be returned not in Context" in {
      the[NoSuchElementException] thrownBy {
        Returns(personB).toQuery(context)
      } should have message "One or more of the elements to be returned are not in Context!"
    }
    "return aliased elements" in {
      Returns(personA -> "person", departmentA -> "department")
        .toQuery(context) shouldBe "RETURN a0 as person,a1 as department"
    }
    "return non-aliased and aliased elements in a single return" in {
      Returns(personA, departmentA -> "department").toQuery(context) shouldBe "RETURN a0,a1 as department"
    }
    "return aliased elements for a property" in {
      Returns(personA('name) -> "personName", departmentA -> "department")
        .toQuery(context) shouldBe "RETURN a0.name as personName,a1 as department"
    }
    "return aliased elements for multiple properties" in {
      Returns(personA('name) -> "name", personA('age) -> "age", departmentA('name) -> "departmentName")
        .toQuery(context) shouldBe "RETURN a0.name as name,a0.age as age,a1.name as departmentName"
    }
    "should throw if multiple properties tried to be aliased" in {
      the[AssertionError] thrownBy {
        Returns(personA('name, 'age) -> "name").toQuery(context)
      } should have message "Alias one property at a time!"
    }
  }
}
