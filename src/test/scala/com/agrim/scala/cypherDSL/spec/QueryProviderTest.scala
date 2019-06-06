package com.agrim.scala.cypherDSL.spec

import com.agrim.scala.cypherDSL.spec.utils.Random._
import com.agrim.scala.cypherDSL.spec.utils.TestClasses.Person
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
        queryProvider.getMatchers(person) shouldBe List("id: {a0_id}", "name: {a0_name}", "age: {a0_age}")
      }
      "create an instance for a given Product for a selected attribute" in {
        queryProvider.getMatchers(person, 'id :: 'name :: HNil) shouldBe List("id: {a0_id}", "name: {a0_name}")
      }
    }
  }

  private def aQueryProvider[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]) = queryProvider
}
