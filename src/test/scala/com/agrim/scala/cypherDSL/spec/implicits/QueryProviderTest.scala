package com.agrim.scala.cypherDSL.spec.implicits

import com.agrim.scala.cypherDSL.spec.Context
import org.scalatest.{Matchers, WordSpec}
import shapeless.HNil
import utils.TestClasses.Person
import utils.Random._

class QueryProviderTest extends WordSpec with Matchers {
  private implicit val context = new Context()
  private val person           = randomize[Person]
  context.add(person)

  "QueryProvider" should {
    val queryProvider = aQueryProvider(person)

    "create an instance for a given Product" in {
      queryProvider.getMatchers(person) shouldBe List("id: {a0_id}", "name: {a0_name}", "age: {a0_age}")
    }
    "create an instance for a given Product for a selected attribute" in {
      queryProvider.getMatchers(person, 'id :: 'name :: HNil) shouldBe List("id: {a0_id}", "name: {a0_name}")
    }
  }

  private def aQueryProvider[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]) = queryProvider
}
