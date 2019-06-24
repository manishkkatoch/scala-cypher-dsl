package me.manishkatoch.scala.cypherDSL.spec

import me.manishkatoch.scala.cypherDSL.spec.utils.Random._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.Person
import org.scalatest.{Matchers, WordSpec}
import shapeless.HNil

class QueryProviderTest extends WordSpec with Matchers {
  private implicit val context = new Context()
  private val person           = randomize[Person]
  context.add(person)

  "QueryProvider" should {
    val queryProvider = aQueryProvider(person)

    "For mandatory product" should {
      "create an instance for a given Product" in {
        queryProvider.getMatchers(person) shouldBe List(
          DSLResult("id: {a0_id}",Map("a0_id" -> person.id)),
          DSLResult("name: {a0_name}",Map("a0_name" -> person.name)),
          DSLResult("age: {a0_age}",Map("a0_age" -> person.age)))
      }
      "create an instance for a given Product for a selected attribute" in {
        queryProvider.getMatchers(person, 'id :: 'name :: HNil) shouldBe List(
          DSLResult("id: {a0_id}",Map("a0_id" -> person.id)),
          DSLResult("name: {a0_name}",Map("a0_name" -> person.name)))
      }
    }
  }

  private def aQueryProvider[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]) = queryProvider
}
