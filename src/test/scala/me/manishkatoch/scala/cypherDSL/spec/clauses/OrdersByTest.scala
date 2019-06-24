package me.manishkatoch.scala.cypherDSL.spec.clauses

import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult}
import me.manishkatoch.scala.cypherDSL.spec.syntax.patterns._
import me.manishkatoch.scala.cypherDSL.spec.utils.Random._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.ImplicitCache._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.{Department, Person}
import org.scalatest.{Matchers, WordSpec}

class OrdersByTest extends WordSpec with Matchers {

  private val personA          = randomize[Person]
  private val personB          = randomize[Person]
  private val departmentA      = randomize[Department]
  private implicit val context = new Context()

  "OrdersBy" should {
    context.add(personA)
    context.add(departmentA)

    "default ordering" should {
      "return query for an element in Context" in {
        OrdersBy(false, personA).toQuery(context) shouldBe DSLResult("ORDER BY a0")
      }

      "return empty statement if no elements passed" in {
        OrdersBy().toQuery(context) shouldBe DSLResult("")
      }

      "return query for more than one element in Context" in {
        OrdersBy(false, personA, departmentA).toQuery(context) shouldBe DSLResult("ORDER BY a0,a1")
      }
      "return elements for a property" in {
        OrdersBy(false, personA('name), departmentA).toQuery(context) shouldBe DSLResult("ORDER BY a0.name,a1")
      }
      "return elements for multiple properties" in {
        OrdersBy(false, personA('name, 'age), departmentA('name))
          .toQuery(context) shouldBe DSLResult("ORDER BY a0.name,a0.age,a1.name")
      }
      "throw if element to be returned not in Context" in {
        the[NoSuchElementException] thrownBy {
          OrdersBy(false, personB).toQuery(context)
        } should have message "One or more of the elements to be returned are not in Context!"
      }
    }
    "DESC ordering" should {
      "return query for an element in Context" in {
        OrdersBy(true, personA).toQuery(context) shouldBe DSLResult("ORDER BY a0 DESC")
      }

      "return empty statement if no elements passed" in {
        OrdersBy().toQuery(context) shouldBe DSLResult("")
      }

      "return query for more than one element in Context" in {
        OrdersBy(true, personA, departmentA).toQuery(context) shouldBe DSLResult("ORDER BY a0,a1 DESC")
      }
      "return elements for a property" in {
        OrdersBy(true, personA('name), departmentA).toQuery(context) shouldBe DSLResult("ORDER BY a0.name,a1 DESC")
      }
      "return elements for multiple properties" in {
        OrdersBy(true, personA('name, 'age), departmentA('name))
          .toQuery(context) shouldBe DSLResult("ORDER BY a0.name,a0.age,a1.name DESC")
      }
      "throw if element to be returned not in Context" in {
        the[NoSuchElementException] thrownBy {
          OrdersBy(true, personB).toQuery(context)
        } should have message "One or more of the elements to be returned are not in Context!"
      }
    }
  }
}
