package me.manishkatoch.scala.cypherDSL.spec.entities

import me.manishkatoch.scala.cypherDSL.spec.utils.Random.randomize
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.{HeadOfDepartment, LocatedIn, Person, WorksIn}
import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult}
import org.scalatest.{Matchers, WordSpec}
import shapeless.HNil

import scala.reflect.runtime.universe.weakTypeOf

class CypherTypeTest extends WordSpec with Matchers {
  "NodeType" should {
    val personType = weakTypeOf[Person]
    ".toQuery" should {
      "provide query string when accessed for first time" in {
        val nodeType = NodeType(personType)
        nodeType.toQuery() shouldBe DSLResult("(a0:Person)")
      }
      "provide query string when accessed for second or more time" in {
        val nodeType = NodeType(personType)
        val context  = new Context()
        nodeType.toQuery(context) shouldBe DSLResult("(a0:Person)")
        nodeType.toQuery(context) shouldBe DSLResult("(a0)")
        nodeType.toQuery(context) shouldBe DSLResult("(a0)")
      }
    }
  }
  "RelationType" should {
    val worksInType = weakTypeOf[WorksIn]
    ".toQuery" should {
      "provide query string when accessed for first time" in {
        val nodeType = RelationType(worksInType)
        nodeType.toQuery() shouldBe DSLResult("[a0:WORKS_IN]")
      }
      "provide query string when accessed for second or more time" in {
        val nodeType = RelationType(worksInType)
        val context  = new Context()
        nodeType.toQuery(context) shouldBe DSLResult("[a0:WORKS_IN]")
        nodeType.toQuery(context) shouldBe DSLResult("[a0]")
        nodeType.toQuery(context) shouldBe DSLResult("[a0]")
      }
      "provide query string for multiple relationships type" in {
        val nodeType = RelationType(tpe = worksInType).or(RelationType(weakTypeOf[HeadOfDepartment]))
        nodeType.toQuery() shouldBe DSLResult("[a0:WORKS_IN|:HEAD_OF_DEPARTMENT]")
      }
      "provide query string for multiple relationships instance" in {
        val headOfDepartment: HeadOfDepartment = randomize[HeadOfDepartment]
        val nodeType = RelationType(tpe = worksInType).or(headOfDepartment, HNil)
        nodeType.toQuery() shouldBe DSLResult(
          "[a0:WORKS_IN|:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}]",
          Map("a1_id" -> headOfDepartment.id, "a1_name" -> headOfDepartment.name)
        )
      }
      "variable length relations" should {
        "provide [A*2..3] query string" in {
          val nodeType = RelationType(worksInType, VariableLengthRelation(2, 3))
          val context  = new Context()
          nodeType.toQuery(context) shouldBe DSLResult("[a0:WORKS_IN*2..3]")
        }
        "provide [A*2] query string" in {
          val nodeType = RelationType(worksInType, VariableLengthRelation(2))
          val context  = new Context()
          nodeType.toQuery(context) shouldBe DSLResult("[a0:WORKS_IN*2]")
        }
      }
    }
  }
  "MultiRelationType" should {
    val worksInType = weakTypeOf[WorksIn]
    ".toQuery" should {
      "provide query string for multiple relationships type" in {
        val orRelations =
          List(RelationTypeOrInstance(weakTypeOf[HeadOfDepartment]), RelationTypeOrInstance(weakTypeOf[LocatedIn]))
        val nodeType = MultiRelationType(orRelations)
        nodeType.toQuery() shouldBe DSLResult("[a0:HEAD_OF_DEPARTMENT|:LOCATED_IN]")
      }
      "provide query string when accessed for second or more time" in {
        val orRelations =
          List(RelationTypeOrInstance(weakTypeOf[HeadOfDepartment]), RelationTypeOrInstance(weakTypeOf[LocatedIn]))
        val nodeType = MultiRelationType(orRelations)
        val context  = new Context()
        nodeType.toQuery(context) shouldBe DSLResult("[a0:HEAD_OF_DEPARTMENT|:LOCATED_IN]")
        nodeType.toQuery(context) shouldBe DSLResult("[a0]")
        nodeType.toQuery(context) shouldBe DSLResult("[a0]")
      }
      "provide query string for multiple relationships instance" in {
        val headOfDepartment: HeadOfDepartment = randomize[HeadOfDepartment]
        val locatedIn: LocatedIn               = randomize[LocatedIn]
        val orRelations =
          List(RelationTypeOrInstance(worksInType),
            RelationTypeOrInstance(Relationship(headOfDepartment, HNil)),
            RelationTypeOrInstance(Relationship(locatedIn, HNil)))
        val nodeType = MultiRelationType(orRelations)

        nodeType.toQuery() shouldBe DSLResult(
          "[a0:WORKS_IN|:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}|:LOCATED_IN {area: {a2_area}}]",
          Map("a1_id" -> headOfDepartment.id, "a1_name" -> headOfDepartment.name, "a2_area" -> locatedIn.area)
        )
      }
      "provide query string for multiple relationships instance and type" in {
        val locatedIn: LocatedIn = randomize[LocatedIn]
        val orRelations =
          List(RelationTypeOrInstance(worksInType),
            RelationTypeOrInstance(weakTypeOf[HeadOfDepartment]),
            RelationTypeOrInstance(Relationship(locatedIn, HNil)))
        val nodeType = MultiRelationType(orRelations)

        nodeType.toQuery() shouldBe DSLResult("[a0:WORKS_IN|:HEAD_OF_DEPARTMENT|:LOCATED_IN {area: {a1_area}}]",
          Map("a1_area" -> locatedIn.area))
      }
      "variable length relations" should {
        "provide [A*2..3] query string" in {
          val nodeType = RelationType(worksInType, VariableLengthRelation(2, 3))
          val context  = new Context()
          nodeType.toQuery(context) shouldBe DSLResult("[a0:WORKS_IN*2..3]")
        }
        "provide [A*2] query string" in {
          val nodeType = RelationType(worksInType, VariableLengthRelation(2))
          val context  = new Context()
          nodeType.toQuery(context) shouldBe DSLResult("[a0:WORKS_IN*2]")
        }
      }
    }
  }

}
