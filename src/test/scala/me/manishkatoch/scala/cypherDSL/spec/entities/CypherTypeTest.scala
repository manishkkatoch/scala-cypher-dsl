package me.manishkatoch.scala.cypherDSL.spec.entities

import me.manishkatoch.scala.cypherDSL.spec.Context
import me.manishkatoch.scala.cypherDSL.spec.utils.Random.randomize
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.ImplicitCache._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.{HeadOfDepartment, LocatedIn, Person, WorksIn}
import org.scalatest.{Matchers, WordSpec}
import shapeless.HNil

import scala.reflect.runtime.universe.weakTypeOf

class CypherTypeTest extends WordSpec with Matchers {
  "NodeType" should {
    val personType = weakTypeOf[Person]
    ".toQuery" should {
      "provide query string when accessed for first time" in {
        val nodeType = NodeType(personType)
        nodeType.toQuery() shouldBe "(a0:Person)"
      }
      "provide query string when accessed for second or more time" in {
        val nodeType = NodeType(personType)
        val context  = new Context()
        nodeType.toQuery(context) shouldBe "(a0:Person)"
        nodeType.toQuery(context) shouldBe "(a0)"
        nodeType.toQuery(context) shouldBe "(a0)"
      }
    }
  }
  "RelationType" should {
    val worksInType = weakTypeOf[WorksIn]
    ".toQuery" should {
      "provide query string when accessed for first time" in {
        val nodeType = RelationType(worksInType)
        nodeType.toQuery() shouldBe "[a0:WORKS_IN]"
      }
      "provide query string when accessed for second or more time" in {
        val nodeType = RelationType(worksInType)
        val context  = new Context()
        nodeType.toQuery(context) shouldBe "[a0:WORKS_IN]"
        nodeType.toQuery(context) shouldBe "[a0]"
        nodeType.toQuery(context) shouldBe "[a0]"
      }
      "provide query string for multiple relationships type" in {
        val orRelations =
          List(RelationTypeOrInstance(weakTypeOf[HeadOfDepartment]), RelationTypeOrInstance(weakTypeOf[LocatedIn]))
        val nodeType = RelationType(tpe = worksInType, orRelations = orRelations)
        nodeType.toQuery() shouldBe "[a0:WORKS_IN|:HEAD_OF_DEPARTMENT|:LOCATED_IN]"
      }
      "provide query string for multiple relationships instance" in {
        val headOfDepartment: HeadOfDepartment = randomize[HeadOfDepartment]
        val locatedIn: LocatedIn               = randomize[LocatedIn]
        val orRelations =
          List(RelationTypeOrInstance(Relationship(headOfDepartment, HNil)),
               RelationTypeOrInstance(Relationship(locatedIn, HNil)))
        val nodeType = RelationType(tpe = worksInType, orRelations = orRelations)
        nodeType.toQuery() shouldBe "[a2:WORKS_IN|:HEAD_OF_DEPARTMENT {id: {a0_id},name: {a0_name}}|:LOCATED_IN {area: {a1_area}}]"
      }
      "provide query string for multiple relationships instance and type" in {
        val locatedIn: LocatedIn = randomize[LocatedIn]
        val orRelations =
          List(RelationTypeOrInstance(weakTypeOf[HeadOfDepartment]),
               RelationTypeOrInstance(Relationship(locatedIn, HNil)))
        val nodeType = RelationType(tpe = worksInType, orRelations = orRelations)
        nodeType.toQuery() shouldBe "[a1:WORKS_IN|:HEAD_OF_DEPARTMENT|:LOCATED_IN {area: {a0_area}}]"
      }
      "variable length relations" should {
        "provide [A*2..3] query string" in {
          val nodeType = RelationType(worksInType, VariableLengthRelation(2, 3))
          val context  = new Context()
          nodeType.toQuery(context) shouldBe "[a0:WORKS_IN*2..3]"
        }
        "provide [A*2] query string" in {
          val nodeType = RelationType(worksInType, VariableLengthRelation(2))
          val context  = new Context()
          nodeType.toQuery(context) shouldBe "[a0:WORKS_IN*2]"
        }
      }
    }
  }

}
