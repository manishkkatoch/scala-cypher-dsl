package com.agrim.scala.cypherDSL

import com.agrim.scala.cypherDSL.spec.Context
import com.agrim.scala.cypherDSL.spec.utils.Random._
import com.agrim.scala.cypherDSL.spec.utils.TestClasses.ImplicitCache._
import com.agrim.scala.cypherDSL.spec.utils.TestClasses._
import com.agrim.scala.cypherDSL.syntax._
import org.scalatest.{Matchers, WordSpec}

class SyntaxTest extends WordSpec with Matchers {
  private val person: Person                     = randomize[Person]
  private val person2: Person                    = randomize[Person]
  private val department: Department             = randomize[Department]
  private val worksIn: WorksIn                   = randomize[WorksIn]
  private val locatedIn: LocatedIn               = randomize[LocatedIn]
  private val region: Region                     = randomize[Region]
  private val headOfDepartment: HeadOfDepartment = randomize[HeadOfDepartment]

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
        path.toQuery shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})"
      }
      "provide (A) -[R]-> (B) grammar" in {
        implicit val context: Context = new Context()

        val path = person -| worksIn |-> department
        path.toQuery shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})"
      }
      "provide (A) <-[R]- (B) grammar" in {
        implicit val context: Context = new Context()

        val path = person <-| worksIn |- department
        path.toQuery shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})"
      }
      "provide (A)-[R]-(B)-[R2]-(A2) grammar" in {
        implicit val context = new Context()

        val path = person -| worksIn |- department -| locatedIn |- region
        path.toQuery shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})-[a3:LOCATED_IN {}]-(a4:Region {name: {a4_name}})"
      }
      "provide (A)-[R]->(B)-[R2]->(A2) grammar" in {
        implicit val context = new Context()

        val path = person -| worksIn |-> department -| locatedIn |-> region
        path.toQuery shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})-[a3:LOCATED_IN {}]->(a4:Region {name: {a4_name}})"
      }
      "provide (A)<-[R]-(B)<-[R2]-(A2) grammar" in {
        implicit val context = new Context()

        val path = person <-| worksIn |- department <-| locatedIn |- region
        path.toQuery shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})<-[a3:LOCATED_IN {}]-(a4:Region {name: {a4_name}})"
      }
      "provide (A)<-[R]-(B)-[R2]->(A2) grammar" in {
        implicit val context = new Context()

        val path = person <-| worksIn |- department -| headOfDepartment |-> person2
        path.toQuery shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})-[a3:HEAD_OF_DEPARTMENT {id: {a3_id},name: {a3_name}}]->(a4:Person {id: {a4_id},name: {a4_name},age: {a4_age}})"
      }
      "provide (A)-[R]->(B)<-[R2]-(A2) grammar" in {
        implicit val context = new Context()

        val path = person2 -| headOfDepartment |-> department <-| worksIn |- person
        path.toQuery shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}]->(a2:Department {id: {a2_id},name: {a2_name}})<-[a3:WORKS_IN {sinceDays: {a3_sinceDays}}]-(a4:Person {id: {a4_id},name: {a4_name},age: {a4_age}})"
      }
      "provide (A)<-[R]-(B)<-[R2]-(A2)-->(C) grammar" in {
        implicit val context = new Context()

        val path = person <-| worksIn |- department <-| locatedIn |- region --> person2
        path.toQuery shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})<-[a3:LOCATED_IN {}]-(a4:Region {name: {a4_name}})-->(a5:Person {id: {a5_id},name: {a5_name},age: {a5_age}})"
      }
    }
    "when in context" should {
      implicit val context: Context = new Context()
      context.add(person)
      context.add(department)
      context.add(worksIn)
      context.add(locatedIn)
      context.add(region)
      context.add(headOfDepartment)
      context.add(person2)

      "provide (A)--(B) grammar" in {
        val path = person -- department
        path.toQuery shouldBe "(a0)--(a1)"
      }
      "provide (A)-->(B) grammar" in {
        val path = person --> department
        path.toQuery shouldBe "(a0)-->(a1)"
      }
      "provide (A)<--(B) grammar" in {
        val path = person <-- department
        path.toQuery shouldBe "(a0)<--(a1)"
      }
      "provide (A)-[R]-(B) grammar" in {
        val path = person -| worksIn |- department
        path.toQuery shouldBe "(a0)-[a2]-(a1)"
      }
      "provide (A)-[R]->(B) grammar" in {
        val path = person -| worksIn |-> department
        path.toQuery shouldBe "(a0)-[a2]->(a1)"
      }
      "provide (A)<-[R]-(B) grammar" in {
        val path = person <-| worksIn |- department
        path.toQuery shouldBe "(a0)<-[a2]-(a1)"
      }
      "provide (A)-[R]-(B)-[R2]-(A2) grammar" in {
        val path = person -| worksIn |- department -| locatedIn |- region
        path.toQuery shouldBe "(a0)-[a2]-(a1)-[a3]-(a4)"
      }
      "provide (A)-[R]->(B)-[R2]->(A2) grammar" in {
        val path = person -| worksIn |-> department -| locatedIn |-> region
        path.toQuery shouldBe "(a0)-[a2]->(a1)-[a3]->(a4)"
      }
      "provide (A)<-[R]-(B)<-[R2]-(A2) grammar" in {
        val path = person <-| worksIn |- department <-| locatedIn |- region
        path.toQuery shouldBe "(a0)<-[a2]-(a1)<-[a3]-(a4)"
      }
      "provide (A)<-[R]-(B)-[R2]->(A2) grammar" in {
        val path = person <-| worksIn |- department -| headOfDepartment |-> person2
        path.toQuery shouldBe "(a0)<-[a2]-(a1)-[a5]->(a6)"
      }
      "provide (A)-[R]->(B)<-[R2]-(A2) grammar" in {
        val path = person2 -| headOfDepartment |-> department <-| worksIn |- person
        path.toQuery shouldBe "(a6)-[a5]->(a1)<-[a2]-(a0)"
      }
      "provide (A)<-[R]-(B)<-[R2]-(A2)-->(C) grammar" in {
        val path = person <-| worksIn |- department <-| locatedIn |- region --> person2
        path.toQuery shouldBe "(a0)<-[a2]-(a1)<-[a3]-(a4)-->(a6)"
      }
    }
  }

}
