package com.agrim.scala.cypherDSL.spec
import com.agrim.scala.cypherDSL.spec.implicits.Products._
import com.agrim.scala.cypherDSL.spec.implicits.QueryProvider
import org.scalatest.{Matchers, WordSpec}
import utils.Random._
import utils.TestClasses.{Department, Person, WorksIn}

class DSLTest extends WordSpec with Matchers {
  private val person: Person         = randomize[Person]
  private val department: Department = randomize[Department]
  private val worksIn: WorksIn       = randomize[WorksIn]
  implicit val personQp              = aQueryProvider[Person]
  implicit val departmentQp          = aQueryProvider[Department]
  implicit val worksInQp             = aQueryProvider[WorksIn]

  "DSL" should {
    "with fresh context" should {
      "provide (A) -- (B) grammar" in {
        implicit val context: Context = new Context()

        val path = person -- department
        path.toQuery shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})--(a1:Department {id: {a1_id},name: {a1_name}})"
      }
      "provide (A) --> (B) grammar" in {
        implicit val context: Context = new Context()

        val path = person --> department
        path.toQuery shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-->(a1:Department {id: {a1_id},name: {a1_name}})"
      }
      "provide (A) <-- (B) grammar" in {
        implicit val context: Context = new Context()

        val path = person <-- department
        path.toQuery shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<--(a1:Department {id: {a1_id},name: {a1_name}})"
      }
      "provide (A) -[R]- (B) grammar" in {
        implicit val context: Context = new Context()

        val path = person -| worksIn |- department
        path.toQuery shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WorksIn {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})"
      }
      "provide (A) -[R]-> (B) grammar" in {
        implicit val context: Context = new Context()

        val path = person -| worksIn |-> department
        path.toQuery shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WorksIn {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})"
      }
      "provide (A) <-[R]- (B) grammar" in {
        implicit val context: Context = new Context()

        val path = person <-| worksIn |- department
        path.toQuery shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:WorksIn {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})"
      }
    }
  }
  private def aQueryProvider[T <: Product](implicit queryProvider: QueryProvider[T]) = queryProvider

}
