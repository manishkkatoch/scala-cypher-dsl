package com.agrim.scala.cypherDSL.spec.implicits

import com.agrim.scala.cypherDSL.spec.{Context, QueryProvider}
import org.scalatest.{Matchers, WordSpec}
import shapeless.HNil
import com.agrim.scala.cypherDSL.spec.utils.TestClasses.Person
import com.agrim.scala.cypherDSL.spec.utils.Random._

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

    "For optional product" should {
      "create instance for a Some Product" in {
        val qp = QueryProvider.optional[Person]
        qp.getMatchers(Option(person)) shouldBe List("id: {a0_id}", "name: {a0_name}", "age: {a0_age}")
      }
      "create instance for a Some Product for a selected attribute" in {
        val qp = QueryProvider.optional[Person]
        qp.getMatchers(Option(person), 'id :: 'name :: HNil) shouldBe List("id: {a0_id}", "name: {a0_name}")
      }
      "create instance for a None Product" in {
        val qp = QueryProvider.optional[Person]
        qp.getMatchers(None) shouldBe List()
      }
      "create instance for a None Product for a selected attribute" in {
        val qp = QueryProvider.optional[Person]
        qp.getMatchers(None, 'id :: 'name :: HNil) shouldBe List()
      }
    }

  }

  private def aQueryProvider[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]) = queryProvider
}
