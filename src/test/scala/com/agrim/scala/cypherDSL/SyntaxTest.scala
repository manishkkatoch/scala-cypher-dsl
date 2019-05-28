package com.agrim.scala.cypherDSL

import com.agrim.scala.cypherDSL.spec.utils.Random._
import com.agrim.scala.cypherDSL.spec.utils.TestClasses.ImplicitCache._
import com.agrim.scala.cypherDSL.spec.utils.TestClasses._
import com.agrim.scala.cypherDSL.spec.{Context, Path}
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

  "Syntax" should {

    "RichNode" should {
      val tenPropClass = randomize[TenPropClass]
      val tenPropNode  = RichNode(tenPropClass)
      "provide Node for all attribute" in {
        tenPropNode.apply
          .toQuery() shouldBe "(a0:TenPropClass {prop1: {a0_prop1},prop2: {a0_prop2},prop3: {a0_prop3},prop4: {a0_prop4},prop5: {a0_prop5},prop6: {a0_prop6},prop7: {a0_prop7},prop8: {a0_prop8},prop9: {a0_prop9},prop10: {a0_prop10}})"
      }
      "provide Node for 1 attribute" in {
        tenPropNode('prop1).toQuery() shouldBe "(a0:TenPropClass {prop1: {a0_prop1}})"
      }
      "provide Node for 2 attribute" in {
        tenPropNode('prop1, 'prop2).toQuery() shouldBe "(a0:TenPropClass {prop1: {a0_prop1},prop2: {a0_prop2}})"
      }
      "provide Node for 3 attribute" in {
        tenPropNode('prop1, 'prop2, 'prop3)
          .toQuery() shouldBe "(a0:TenPropClass {prop1: {a0_prop1},prop2: {a0_prop2},prop3: {a0_prop3}})"
      }
      "provide Node for 4 attribute" in {
        tenPropNode('prop1, 'prop2, 'prop3, 'prop4)
          .toQuery() shouldBe "(a0:TenPropClass {prop1: {a0_prop1},prop2: {a0_prop2},prop3: {a0_prop3},prop4: {a0_prop4}})"
      }
      "provide Node for 5 attribute" in {
        tenPropNode('prop1, 'prop2, 'prop3, 'prop4, 'prop5)
          .toQuery() shouldBe "(a0:TenPropClass {prop1: {a0_prop1},prop2: {a0_prop2},prop3: {a0_prop3},prop4: {a0_prop4},prop5: {a0_prop5}})"
      }
      "provide Node for 6 attribute" in {
        tenPropNode('prop1, 'prop2, 'prop3, 'prop4, 'prop5, 'prop6)
          .toQuery() shouldBe "(a0:TenPropClass {prop1: {a0_prop1},prop2: {a0_prop2},prop3: {a0_prop3},prop4: {a0_prop4},prop5: {a0_prop5},prop6: {a0_prop6}})"
      }
      "provide Node for 7 attribute" in {
        tenPropNode('prop1, 'prop2, 'prop3, 'prop4, 'prop5, 'prop6, 'prop7)
          .toQuery() shouldBe "(a0:TenPropClass {prop1: {a0_prop1},prop2: {a0_prop2},prop3: {a0_prop3},prop4: {a0_prop4},prop5: {a0_prop5},prop6: {a0_prop6},prop7: {a0_prop7}})"
      }
      "provide Node for 8 attribute" in {
        tenPropNode('prop1, 'prop2, 'prop3, 'prop4, 'prop5, 'prop6, 'prop7, 'prop8)
          .toQuery() shouldBe "(a0:TenPropClass {prop1: {a0_prop1},prop2: {a0_prop2},prop3: {a0_prop3},prop4: {a0_prop4},prop5: {a0_prop5},prop6: {a0_prop6},prop7: {a0_prop7},prop8: {a0_prop8}})"
      }
      "provide Node for 9 attribute" in {
        tenPropNode('prop1, 'prop2, 'prop3, 'prop4, 'prop5, 'prop6, 'prop7, 'prop8, 'prop9)
          .toQuery() shouldBe "(a0:TenPropClass {prop1: {a0_prop1},prop2: {a0_prop2},prop3: {a0_prop3},prop4: {a0_prop4},prop5: {a0_prop5},prop6: {a0_prop6},prop7: {a0_prop7},prop8: {a0_prop8},prop9: {a0_prop9}})"
      }
      "provide Node for 10 attribute" in {
        tenPropNode('prop1, 'prop2, 'prop3, 'prop4, 'prop5, 'prop6, 'prop7, 'prop8, 'prop9, 'prop10)
          .toQuery() shouldBe "(a0:TenPropClass {prop1: {a0_prop1},prop2: {a0_prop2},prop3: {a0_prop3},prop4: {a0_prop4},prop5: {a0_prop5},prop6: {a0_prop6},prop7: {a0_prop7},prop8: {a0_prop8},prop9: {a0_prop9},prop10: {a0_prop10}})"
      }

    }
    "with fresh context" should {
      "provide (A) -- (B) grammar" in {
        implicit val context: Context = new Context()

        val path = person -- department
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})--(a1:Department {id: {a1_id},name: {a1_name}})"
      }
      "provide (A) --> (B) grammar" in {
        implicit val context: Context = new Context()

        val path = person --> department
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-->(a1:Department {id: {a1_id},name: {a1_name}})"
      }
      "provide (A) <-- (B) grammar" in {
        implicit val context: Context = new Context()

        val path = person <-- department
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<--(a1:Department {id: {a1_id},name: {a1_name}})"
      }
      "provide (A) -[R]- (B) grammar" in {
        implicit val context: Context = new Context()

        val path = person -| worksIn |- department
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})"
      }
      "provide (A)-[R|S]-(B) grammar" in {
        implicit val context: Context = new Context()
        val path                      = person -| worksIn | headOfDepartment |- department
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a2:WORKS_IN {sinceDays: {a2_sinceDays}}|a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}]-(a3:Department {id: {a3_id},name: {a3_name}})"
      }
      "provide (A) -[R]-> (B) grammar" in {
        implicit val context: Context = new Context()

        val path = person -| worksIn |-> department
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})"
      }
      "provide (A)-[R|S]->(B) grammar" in {
        implicit val context: Context = new Context()
        val path                      = person -| worksIn | headOfDepartment |-> department
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a2:WORKS_IN {sinceDays: {a2_sinceDays}}|a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}]->(a3:Department {id: {a3_id},name: {a3_name}})"
      }
      "provide (A)-[*n..m]->(B) grammar" in {
        val context: Context = new Context()
        val path: Path       = person -|* (1 to 4) |-> department
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[*1..4]->(a1:Department {id: {a1_id},name: {a1_name}})"
      }
      "provide (A)-[*]->(B) grammar" in {
        val context: Context = new Context()
        val path             = person -|* () |-> department
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[*]->(a1:Department {id: {a1_id},name: {a1_name}})"
      }
      "provide (A) <-[R]- (B) grammar" in {
        implicit val context: Context = new Context()

        val path = person <-| worksIn |- department
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})"
      }
      "provide (A) <-[R|S]- (B) grammar" in {
        implicit val context: Context = new Context()

        val path = person <-| worksIn | headOfDepartment |- department
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a2:WORKS_IN {sinceDays: {a2_sinceDays}}|a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}]-(a3:Department {id: {a3_id},name: {a3_name}})"
      }
      "provide (A)-[R]-(B)-[R2]-(A2) grammar" in {
        implicit val context = new Context()

        val path = person -| worksIn |- department -| locatedIn |- region
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})-[a3:LOCATED_IN {}]-(a4:Region {name: {a4_name}})"
      }
      "provide (A)-[R]->(B)-[R2]->(A2) grammar" in {
        implicit val context = new Context()

        val path = person -| worksIn |-> department -| locatedIn |-> region
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})-[a3:LOCATED_IN {}]->(a4:Region {name: {a4_name}})"
      }
      "provide (A)<-[R]-(B)<-[R2]-(A2) grammar" in {
        implicit val context = new Context()

        val path = person <-| worksIn |- department <-| locatedIn |- region
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})<-[a3:LOCATED_IN {}]-(a4:Region {name: {a4_name}})"
      }
      "provide (A)<-[R]-(B)-[R2]->(A2) grammar" in {
        implicit val context = new Context()

        val path = person <-| worksIn |- department -| headOfDepartment |-> person2
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})-[a3:HEAD_OF_DEPARTMENT {id: {a3_id},name: {a3_name}}]->(a4:Person {id: {a4_id},name: {a4_name},age: {a4_age}})"
      }
      "provide (A)-[R]->(B)<-[R2]-(A2) grammar" in {
        implicit val context = new Context()

        val path = person2 -| headOfDepartment |-> department <-| worksIn |- person
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}]->(a2:Department {id: {a2_id},name: {a2_name}})<-[a3:WORKS_IN {sinceDays: {a3_sinceDays}}]-(a4:Person {id: {a4_id},name: {a4_name},age: {a4_age}})"
      }
      "provide (A)<-[R]-(B)<-[R2]-(A2)-->(C) grammar" in {
        implicit val context = new Context()

        val path = person <-| worksIn |- department <-| locatedIn |- region --> person2
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})<-[a3:LOCATED_IN {}]-(a4:Region {name: {a4_name}})-->(a5:Person {id: {a5_id},name: {a5_name},age: {a5_age}})"
      }
      "provide (A)<-[*]-(B)<-[*1..5]-(A2)-[*]->(C) grammar" in {
        val context = new Context()
        val path    = person <-|* () |- department <-|* (1 to 5) |- region -|* () |-> person2
        path.toQuery(context) shouldBe "(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[*]-(a1:Department {id: {a1_id},name: {a1_name}})<-[*1..5]-(a2:Region {name: {a2_name}})-[*]->(a3:Person {id: {a3_id},name: {a3_name},age: {a3_age}})"
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
      "Path" should {
        "provide (A)--(B) grammar" in {
          val path = person -- department
          path.toQuery(context) shouldBe "(a0)--(a1)"
        }
        "provide (A)-->(B) grammar" in {
          val path = person --> department
          path.toQuery(context) shouldBe "(a0)-->(a1)"
        }
        "provide (A)<--(B) grammar" in {
          val path = person <-- department
          path.toQuery(context) shouldBe "(a0)<--(a1)"
        }
        "provide (A)-[R]-(B) grammar" in {
          val path = person -| worksIn |- department
          path.toQuery(context) shouldBe "(a0)-[a2]-(a1)"
        }
        "provide (A)-[R]->(B) grammar" in {
          val path = person -| worksIn |-> department
          path.toQuery(context) shouldBe "(a0)-[a2]->(a1)"
        }
        "provide (A)-[R|S]->(B) grammar" in {
          val path = person -| worksIn | headOfDepartment |-> department
          path.toQuery(context) shouldBe "(a0)-[a2|a5]->(a1)"
        }
        "provide (A)-[*n..m]->(B) grammar" in {
          val path: Path = person -|* (1 to 4) |-> department
          path.toQuery(context) shouldBe "(a0)-[*1..4]->(a1)"
        }
        "provide (A)-[*]->(B) grammar" in {
          val path = person -|* () |-> department
          path.toQuery(context) shouldBe "(a0)-[*]->(a1)"
        }
        "provide (A)<-[R]-(B) grammar" in {
          val path = person <-| worksIn |- department
          path.toQuery(context) shouldBe "(a0)<-[a2]-(a1)"
        }
        "provide (A)-[R]-(B)-[R2]-(A2) grammar" in {
          val path = person -| worksIn |- department -| locatedIn |- region
          path.toQuery(context) shouldBe "(a0)-[a2]-(a1)-[a3]-(a4)"
        }
        "provide (A)-[R]->(B)-[R2]->(A2) grammar" in {
          val path = person -| worksIn |-> department -| locatedIn |-> region
          path.toQuery(context) shouldBe "(a0)-[a2]->(a1)-[a3]->(a4)"
        }
        "provide (A)<-[R]-(B)<-[R2]-(A2) grammar" in {
          val path = person <-| worksIn |- department <-| locatedIn |- region
          path.toQuery(context) shouldBe "(a0)<-[a2]-(a1)<-[a3]-(a4)"
        }
        "provide (A)<-[R]-(B)-[R2]->(A2) grammar" in {
          val path = person <-| worksIn |- department -| headOfDepartment |-> person2
          path.toQuery(context) shouldBe "(a0)<-[a2]-(a1)-[a5]->(a6)"
        }
        "provide (A)-[R]->(B)<-[R2]-(A2) grammar" in {
          val path = person2 -| headOfDepartment |-> department <-| worksIn |- person
          path.toQuery(context) shouldBe "(a6)-[a5]->(a1)<-[a2]-(a0)"
        }
        "provide (A)<-[R]-(B)<-[R2]-(A2)-->(C) grammar" in {
          val path = person <-| worksIn |- department <-| locatedIn |- region --> person2
          path.toQuery(context) shouldBe "(a0)<-[a2]-(a1)<-[a3]-(a4)-->(a6)"
        }
        "provide (A)<-[*]-(B)<-[*1..5]-(A2)-[*]->(C) grammar" in {
          val path = person <-|* () |- department <-|* (1 to 5) |- region -|* () |-> person2
          path.toQuery(context) shouldBe "(a0)<-[*]-(a1)<-[*1..5]-(a4)-[*]->(a6)"
        }
      }
    }
    "V1" should {
      import v1._
      "provide basic query " in {
        cypher
          .MATCH(person)
          .RETURN(person)
          .toQuery(new Context()) shouldBe
          """MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})
            |RETURN a0""".stripMargin
      }
      "provide basic query for a node" in {
        cypher
          .MATCH(person('name))
          .RETURN(person)
          .toQuery(new Context()) shouldBe
          """MATCH (a0:Person {name: {a0_name}})
            |RETURN a0""".stripMargin
      }
      "provide basic query for an optional match on node" in {
        cypher
          .OPTIONAL_MATCH(person('name))
          .RETURN(person)
          .toQuery(new Context()) shouldBe
          """OPTIONAL MATCH (a0:Person {name: {a0_name}})
            |RETURN a0""".stripMargin
      }
      "provide basic query for a path" in {
        cypher
          .MATCH(person('name) -| worksIn |-> department)
          .RETURN(person -> "worker", department -> "dept")
          .toQuery(new Context()) shouldBe
          """MATCH (a0:Person {name: {a0_name}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})
            |RETURN a0 as worker,a2 as dept""".stripMargin
      }
      "provide multi match-query for a path" in {
        val personName = person('name)
        cypher
          .MATCH(personName -| worksIn |-> department)
          .MATCH(department -|* () |-> region)
          .RETURN(person -> "worker", department -> "dept")
          .toQuery(new Context()) shouldBe
          """MATCH (a0:Person {name: {a0_name}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})
            |MATCH (a2)-[*]->(a3:Region {name: {a3_name}})
            |RETURN a0 as worker,a2 as dept""".stripMargin
      }
      "provide optional match query for a path" in {
        val personName = person('name)
        cypher
          .MATCH(personName -| worksIn('sinceDays) |-> department)
          .OPTIONAL_MATCH(department -|* () |-> region)
          .RETURN(person -> "worker", department -> "dept")
          .toQuery(new Context()) shouldBe
          """MATCH (a0:Person {name: {a0_name}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})
            |OPTIONAL MATCH (a2)-[*]->(a3:Region {name: {a3_name}})
            |RETURN a0 as worker,a2 as dept""".stripMargin
      }
      "provide skip limit query for a path" in {
        val personName = person('name)
        cypher
          .MATCH(personName -| worksIn('sinceDays) |-> department)
          .OPTIONAL_MATCH(department -|* () |-> region)
          .SKIP(5)
          .LIMIT(10)
          .RETURN(person -> "worker", department -> "dept")
          .toQuery(new Context()) shouldBe
          """MATCH (a0:Person {name: {a0_name}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})
            |OPTIONAL MATCH (a2)-[*]->(a3:Region {name: {a3_name}})
            |SKIP 5
            |LIMIT 10
            |RETURN a0 as worker,a2 as dept""".stripMargin
      }
      "provide skip limit as return in query for a path" in {
        val personName = person('name)
        cypher
          .MATCH(personName -| worksIn('sinceDays) |-> department)
          .OPTIONAL_MATCH(department -|* () |-> region)
          .RETURN(person -> "worker", department -> "dept")
          .SKIP(5)
          .LIMIT(10)
          .toQuery(new Context()) shouldBe
          """MATCH (a0:Person {name: {a0_name}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})
            |OPTIONAL MATCH (a2)-[*]->(a3:Region {name: {a3_name}})
            |RETURN a0 as worker,a2 as dept
            |SKIP 5
            |LIMIT 10""".stripMargin
      }
      "provide order by return in query for a path" in {
        val personName = person('name)
        cypher
          .MATCH(personName -| worksIn('sinceDays) |-> department)
          .OPTIONAL_MATCH(department -|* () |-> region)
          .RETURN(person('name) -> "workerName", department -> "dept")
          .SKIP(5)
          .LIMIT(10)
          .ORDER_BY(person('name, 'age), department)
          .toQuery(new Context()) shouldBe
          """MATCH (a0:Person {name: {a0_name}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})
            |OPTIONAL MATCH (a2)-[*]->(a3:Region {name: {a3_name}})
            |RETURN a0.name as workerName,a2 as dept
            |SKIP 5
            |LIMIT 10
            |ORDER BY a0.name,a0.age,a2""".stripMargin
      }
      "provide order by desc in query for a path" in {
        val personName = person('name)
        cypher
          .MATCH(personName -| worksIn('sinceDays) |-> department)
          .OPTIONAL_MATCH(department -|* () |-> region)
          .ORDER_BY_DESC(person('name, 'age), department)
          .RETURN(person('name) -> "workerName", department -> "dept")
          .SKIP(5)
          .LIMIT(10)
          .toQuery(new Context()) shouldBe
          """MATCH (a0:Person {name: {a0_name}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})
            |OPTIONAL MATCH (a2)-[*]->(a3:Region {name: {a3_name}})
            |ORDER BY a0.name,a0.age,a2 DESC
            |RETURN a0.name as workerName,a2 as dept
            |SKIP 5
            |LIMIT 10""".stripMargin
      }

      "provide WITH in query for a path" in {
        val personName = person('name)
        cypher
          .MATCH(personName -| worksIn('sinceDays) |-> department)
          .OPTIONAL_MATCH(department -|* () |-> region)
          .WITH(person, region -> "reg")
          .RETURN(person('name) -> "workerName", department -> "dept", region)
          .SKIP(5)
          .LIMIT(10)
          .toQuery(new Context()) shouldBe
          """MATCH (a0:Person {name: {a0_name}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})
            |OPTIONAL MATCH (a2)-[*]->(a3:Region {name: {a3_name}})
            |WITH a0,a3 as reg
            |RETURN a0.name as workerName,a2 as dept,reg
            |SKIP 5
            |LIMIT 10""".stripMargin
      }

    }
  }
}
