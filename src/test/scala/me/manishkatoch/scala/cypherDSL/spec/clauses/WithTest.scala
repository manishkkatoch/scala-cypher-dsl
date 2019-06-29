package me.manishkatoch.scala.cypherDSL.spec.clauses

import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult}
import me.manishkatoch.scala.cypherDSL.spec.syntax.patterns._
import me.manishkatoch.scala.cypherDSL.spec.utils.Random._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.ImplicitCache._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.{Department, Person}
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}

class WithTest extends WordSpec with Matchers with BeforeAndAfterEach {

  private val personA                   = randomize[Person]
  private val personB                   = randomize[Person]
  private val departmentA               = randomize[Department]
  private implicit var context: Context = new Context()

  override def beforeEach(): Unit = {
    context = new Context()
    context.add(personA)
    context.add(departmentA)
  }

  "With" should {

    "WITH query for an element in Context" in {
      With(personA).toQuery(context) shouldBe DSLResult("WITH a0")
    }

    "WITH empty statement if no elements passed" in {
      With().toQuery(context) shouldBe DSLResult.empty
    }

    "WITH query for more than one element in Context" in {
      With(personA, departmentA).toQuery(context) shouldBe DSLResult("WITH a0,a1")
    }
    "WITH elements for a property" in {
      With(personA('name), departmentA).toQuery(context) shouldBe DSLResult("WITH a0.name,a1")
    }
    "WITH elements for multiple properties" in {
      With(personA('name, 'age), departmentA('name)).toQuery(context) shouldBe DSLResult("WITH a0.name,a0.age,a1.name")
    }
    "throw if element to be WITHed not in Context" in {
      the[NoSuchElementException] thrownBy {
        With(personB).toQuery(context)
      } should have message "One or more of the elements to be returned are not in Context!"
    }
    "WITH aliased elements" in {
      With(personA -> "person", departmentA -> "department")
        .toQuery(context) shouldBe DSLResult("WITH a0 as person,a1 as department")
    }
    "WITH non-aliased and aliased elements in a single WITH" in {
      With(personA, departmentA -> "department").toQuery(context) shouldBe DSLResult("WITH a0,a1 as department")
    }
    "WITH aliased elements for a property" in {
      With(personA('name) -> "personName", departmentA -> "department")
        .toQuery(context) shouldBe DSLResult("WITH a0.name as personName,a1 as department")
    }
    "WITH aliased elements for multiple properties" in {
      With(personA('name) -> "name", personA('age) -> "age", departmentA('name) -> "departmentName")
        .toQuery(context) shouldBe DSLResult("WITH a0.name as name,a0.age as age,a1.name as departmentName")
    }
    "WITH aliased elements should return alias going forward" in {
      With(personA -> "p").toQuery(context)
      Returns(personA).toQuery(context) shouldBe DSLResult("RETURN p")
    }
    "should throw if multiple properties tried to be aliased" in {
      the[AssertionError] thrownBy {
        With(personA('name, 'age) -> "name").toQuery(context)
      } should have message "Alias one property at a time!"
    }
  }
}
