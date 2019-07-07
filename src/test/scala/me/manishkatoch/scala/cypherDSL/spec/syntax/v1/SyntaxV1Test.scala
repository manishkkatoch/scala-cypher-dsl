package me.manishkatoch.scala.cypherDSL.spec.syntax.v1

import me.manishkatoch.scala.cypherDSL.spec.operators.Distinct
import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult}
import me.manishkatoch.scala.cypherDSL.spec.syntax.patterns._
import me.manishkatoch.scala.cypherDSL.spec.utils.Random.randomize
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.ImplicitCache._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.{Department, Person, WorksIn}
import org.scalatest.{Matchers, WordSpec}

class SyntaxV1Test extends WordSpec with Matchers {
  val person: Person   = randomize[Person]
  val anyPerson        = any[Person]
  val worksIn: WorksIn = randomize[WorksIn]
  val dept: Department = randomize[Department]

  "MATCH" should {
    "provide query for an instance" in {
      cypher.MATCH(person).toQuery(new Context()) shouldBe DSLResult(
        "MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})",
        Map("a0_id" -> person.id, "a0_name" -> person.name, "a0_age" -> person.age))
    }
    "provide query for a node" in {
      cypher.MATCH(person('name)).toQuery(new Context()) shouldBe DSLResult("MATCH (a0:Person {name: {a0_name}})",
                                                                            Map("a0_name" -> person.name))
    }
    "provide query for a class" in {
      cypher.MATCH(anyPerson).toQuery(new Context()) shouldBe DSLResult("MATCH (a0:Person)")
    }
    "provide query for a path" in {
      cypher.MATCH(person -| worksIn |-> dept).toQuery(new Context()) shouldBe DSLResult(
        "MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})",
        Map("a0_id"        -> person.id,
            "a0_name"      -> person.name,
            "a0_age"       -> person.age,
            "a1_sinceDays" -> worksIn.sinceDays,
            "a2_id"        -> dept.id,
            "a2_name"      -> dept.name)
      )
    }
  }
  "CREATE" should {
    "provide query for an instance" in {
      cypher.CREATE(person).toQuery(new Context()) shouldBe DSLResult(
        "CREATE (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})",
        Map("a0_id" -> person.id, "a0_name" -> person.name, "a0_age" -> person.age))
    }
    "provide query for a node" in {
      cypher.CREATE(person('name)).toQuery(new Context()) shouldBe DSLResult("CREATE (a0:Person {name: {a0_name}})",
                                                                            Map("a0_name" -> person.name))
    }
    "provide query for a class" in {
      cypher.CREATE(anyPerson).toQuery(new Context()) shouldBe DSLResult("CREATE (a0:Person)")
    }
    "provide query for a path" in {
      cypher.CREATE(person -| worksIn |-> dept).toQuery(new Context()) shouldBe DSLResult(
        "CREATE (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})",
        Map("a0_id"        -> person.id,
            "a0_name"      -> person.name,
            "a0_age"       -> person.age,
            "a1_sinceDays" -> worksIn.sinceDays,
            "a2_id"        -> dept.id,
            "a2_name"      -> dept.name)
      )
    }
  }
  "MERGE" should {
    "provide query for an instance" in {
      cypher.MERGE(person).toQuery(new Context()) shouldBe DSLResult(
        "MERGE (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})",
        Map("a0_id" -> person.id, "a0_name" -> person.name, "a0_age" -> person.age))
    }
    "provide query for a node" in {
      cypher.MERGE(person('name)).toQuery(new Context()) shouldBe DSLResult("MERGE (a0:Person {name: {a0_name}})",
        Map("a0_name" -> person.name))
    }
    "provide query for a class" in {
      cypher.MERGE(anyPerson).toQuery(new Context()) shouldBe DSLResult("MERGE (a0:Person)")
    }
    "provide query for a path" in {
      cypher.MERGE(person -| worksIn |-> dept).toQuery(new Context()) shouldBe DSLResult(
        "MERGE (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})",
        Map("a0_id"        -> person.id,
          "a0_name"      -> person.name,
          "a0_age"       -> person.age,
          "a1_sinceDays" -> worksIn.sinceDays,
          "a2_id"        -> dept.id,
          "a2_name"      -> dept.name)
      )
    }
  }
  "DELETE" should {
    "provide query for an instance" in {
      cypher.DELETE(person).toQuery(new Context()) shouldBe DSLResult(
        "DELETE (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})",
        Map("a0_id" -> person.id, "a0_name" -> person.name, "a0_age" -> person.age))
    }
    "provide query for a node" in {
      cypher.DELETE(person('name)).toQuery(new Context()) shouldBe DSLResult("DELETE (a0:Person {name: {a0_name}})",
        Map("a0_name" -> person.name))
    }
    "provide query for a class" in {
      cypher.DELETE(anyPerson).toQuery(new Context()) shouldBe DSLResult("DELETE (a0:Person)")
    }
    "provide query for a path" in {
      cypher.DELETE(person -| worksIn |-> dept).toQuery(new Context()) shouldBe DSLResult(
        "DELETE (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})",
        Map("a0_id"        -> person.id,
          "a0_name"      -> person.name,
          "a0_age"       -> person.age,
          "a1_sinceDays" -> worksIn.sinceDays,
          "a2_id"        -> dept.id,
          "a2_name"      -> dept.name)
      )
    }
  }
  "DETACH_DELETE" should {
    "provide query for an instance" in {
      cypher.DETACH_DELETE(person).toQuery(new Context()) shouldBe DSLResult(
        "DETACH DELETE (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})",
        Map("a0_id" -> person.id, "a0_name" -> person.name, "a0_age" -> person.age))
    }
    "provide query for a node" in {
      cypher.DETACH_DELETE(person('name)).toQuery(new Context()) shouldBe DSLResult("DETACH DELETE (a0:Person {name: {a0_name}})",
        Map("a0_name" -> person.name))
    }
    "provide query for a class" in {
      cypher.DETACH_DELETE(anyPerson).toQuery(new Context()) shouldBe DSLResult("DETACH DELETE (a0:Person)")
    }
    "provide query for a path" in {
      cypher.DETACH_DELETE(person -| worksIn |-> dept).toQuery(new Context()) shouldBe DSLResult(
        "DETACH DELETE (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})",
        Map("a0_id"        -> person.id,
          "a0_name"      -> person.name,
          "a0_age"       -> person.age,
          "a1_sinceDays" -> worksIn.sinceDays,
          "a2_id"        -> dept.id,
          "a2_name"      -> dept.name)
      )
    }
  }
  "SET" should {
    val context = new Context()
    cypher.MATCH(person).toQuery(context)
    cypher.MATCH(anyPerson).toQuery(context)
    "provide query for an instance with setters" in {
      cypher.SET(person, List(person('name) -> "tom")).toQuery(context) shouldBe DSLResult(
        "SET a0 = {a0.name = {a0_name}}",
        Map("a0_name" -> "tom"))
    }
    "provide query for an instance with no setters" in {
      cypher.SET(person, List.empty).toQuery(context) shouldBe DSLResult(
        "SET a0 = {}")
    }
    "provide query for a node" in {
      cypher.SET(person('name) -> "tom", person('age) -> 12).toQuery(context) shouldBe
        DSLResult("SET a0.name = {a0_name},a0.age = {a0_age}",
        Map("a0_name" -> "tom", "a0_age" -> 12))
    }

  }
  "OPTIONAL_MATCH" should {
    "provide query for an instance" in {
      cypher.OPTIONAL_MATCH(person).toQuery(new Context()) shouldBe DSLResult(
        "OPTIONAL MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})",
        Map("a0_id" -> person.id, "a0_name" -> person.name, "a0_age" -> person.age))
    }
    "provide query for a node" in {
      cypher.OPTIONAL_MATCH(person('name)).toQuery(new Context()) shouldBe DSLResult(
        "OPTIONAL MATCH (a0:Person {name: {a0_name}})",
        Map("a0_name" -> person.name))
    }
    "provide query for a class" in {
      cypher.OPTIONAL_MATCH(any[Person]).toQuery(new Context()) shouldBe DSLResult("OPTIONAL MATCH (a0:Person)")
    }
    "provide query for a path" in {
      cypher.OPTIONAL_MATCH(person -| worksIn |-> dept).toQuery(new Context()) shouldBe DSLResult(
        "OPTIONAL MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})",
        Map("a0_id"        -> person.id,
            "a0_name"      -> person.name,
            "a0_age"       -> person.age,
            "a1_sinceDays" -> worksIn.sinceDays,
            "a2_id"        -> dept.id,
            "a2_name"      -> dept.name)
      )
    }
  }
  "SKIP" should {
    "provide query for a given count" in {
      cypher.SKIP(10).toQuery(new Context()) shouldBe DSLResult("SKIP 10")
    }
  }
  "LIMIT" should {
    "provide query for a given count" in {
      cypher.LIMIT(10).toQuery(new Context()) shouldBe DSLResult("LIMIT 10")
    }
  }
  "RETURN" should {
    val context = new Context()
    context.add(person)
    context.add(anyPerson)
    context.add(dept)
    "return query for an element in Context" in {
      val context = new Context()
      context.add(person)
      cypher.RETURN(person).toQuery(context) shouldBe DSLResult("RETURN a0")
    }
    "return query for any element in Context" in {
      val context = new Context()
      context.add(person)
      cypher.MATCH(anyPerson).toQuery(context)
      cypher.RETURN(anyPerson).toQuery(context) shouldBe DSLResult("RETURN a1")
    }
    "return empty statement if no elements passed" in {
      cypher.RETURN().toQuery(context) shouldBe DSLResult.empty
    }
    "return query for more than one element in Context" in {
      val context = new Context()
      context.add(person)
      cypher.MATCH(anyPerson).toQuery(context)
      cypher.RETURN(person, anyPerson).toQuery(context) shouldBe DSLResult("RETURN a0,a1")
    }
    "return elements for a property" in {
      cypher.RETURN(person('name), dept).toQuery(context) shouldBe DSLResult("RETURN a0.name,a2")
    }
    "return elements for multiple properties" in {
      cypher.RETURN(person('name, 'age), dept('name)).toQuery(context) shouldBe DSLResult(
        "RETURN a0.name,a0.age,a2.name")
    }
    "throw if element to be returned not in Context" in {
      the[NoSuchElementException] thrownBy {
        cypher.RETURN(worksIn).toQuery(context)
      } should have message "One or more of the elements to be returned are not in Context!"
    }
    "return aliased elements" in {
      cypher.RETURN(person -> "person", dept -> "department").toQuery(context) shouldBe DSLResult(
        "RETURN a0 as person,a2 as department")
    }
    "return non-aliased and aliased elements in a single return" in {
      cypher.RETURN(person, dept -> "department").toQuery(context) shouldBe DSLResult("RETURN a0,a2 as department")
    }
    "return aliased elements for a property" in {
      cypher.RETURN(person('name) -> "personName", dept -> "department").toQuery(context) shouldBe DSLResult(
        "RETURN a0.name as personName,a2 as department")
    }
    "return aliased elements for multiple properties" in {
      cypher
        .RETURN(person('name) -> "name", person('age) -> "age", dept('name) -> "departmentName")
        .toQuery(context) shouldBe DSLResult("RETURN a0.name as name,a0.age as age,a2.name as departmentName")
    }
    "RETURN DISTINCT aliased elements" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher
        .RETURN(Distinct(person -> "pers", dept -> "department"))
        .toQuery(context) shouldBe DSLResult("RETURN DISTINCT a0 as pers,a1 as department")
    }
    "RETURN DISTINCT  aliased elements for multiple properties" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher
        .RETURN(Distinct(person('name) -> "name", person('age) -> "age", dept('name) -> "departmentName"))
        .toQuery(context) shouldBe DSLResult("RETURN DISTINCT a0.name as name,a0.age as age,a1.name as departmentName")
    }
  }
  "WITH" should {
    val context = new Context()
    context.add(person)
    context.add(dept)
    "WITH query for an element in Context" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher.WITH(person).toQuery(context) shouldBe DSLResult("WITH a0")
    }

    "WITH empty statement if no elements passed" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher.WITH().toQuery(context) shouldBe DSLResult.empty
    }

    "WITH query for more than one element in Context" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher.WITH(person, dept).toQuery(context) shouldBe DSLResult("WITH a0,a1")
    }
    "WITH elements for a property" in {
      cypher.WITH(person('name), dept).toQuery(context) shouldBe DSLResult("WITH a0.name,a1")
    }
    "WITH elements for multiple properties" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher.WITH(person('name, 'age), dept('name)).toQuery(context) shouldBe DSLResult("WITH a0.name,a0.age,a1.name")
    }
    "throw if element to be WITHed not in Context" in {
      the[NoSuchElementException] thrownBy {
        cypher.WITH(worksIn).toQuery(context)
      } should have message "One or more of the elements to be returned are not in Context!"
    }
    "WITH aliased elements" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher.WITH(person -> "person", dept -> "dept").toQuery(context) shouldBe DSLResult(
        "WITH a0 as person,a1 as dept")
    }
    "WITH non-aliased and aliased elements in a single WITH" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher.WITH(person, dept -> "dept").toQuery(context) shouldBe DSLResult("WITH a0,a1 as dept")
    }
    "WITH aliased elements for a property" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher.WITH(person('name) -> "personName", dept -> "dept").toQuery(context) shouldBe DSLResult(
        "WITH a0.name as personName,a1 as dept")
    }
    "WITH aliased elements for multiple properties" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher
        .WITH(person('name) -> "name", person('age) -> "age", dept('name) -> "departmentName")
        .toQuery(context) shouldBe DSLResult("WITH a0.name as name,a0.age as age,a1.name as departmentName")
    }
    "WITH DISTINCT aliased elements" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher
        .WITH(Distinct(person -> "pers", dept -> "department"))
        .toQuery(context) shouldBe DSLResult("WITH DISTINCT a0 as pers,a1 as department")
    }
    "WITH DISTINCT  aliased elements for multiple properties" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher
        .WITH(Distinct(person('name) -> "name"), dept('name) -> "departmentName")
        .RETURN(person('name))
        .toQuery(context) shouldBe DSLResult(
          """WITH DISTINCT a0.name as name,a1.name as departmentName
            |RETURN name""".stripMargin)
    }
    "WITH aliased elements should return alias going forward" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      val anyPerson = any[Person]
      val anyRel = anyRelation
      cypher.MATCH(anyPerson -| anyRel |-> anyNode).toQuery(context)
      cypher.WITH(person -> "p", anyPerson -> "p2", anyRel -> "ar").toQuery(context)
      cypher.RETURN(person, anyPerson, anyRel).toQuery(context) shouldBe DSLResult("RETURN p,p2,ar")
    }
  }
  "ORDER_BY" should {
    val context = new Context()
    context.add(person)
    context.add(dept)

    "return query for an element in Context" in {
      cypher.ORDER_BY(person).toQuery(context) shouldBe DSLResult("ORDER BY a0")
    }

    "return empty statement if no elements passed" in {
      cypher.ORDER_BY().toQuery(context) shouldBe DSLResult.empty
    }

    "return query for more than one element in Context" in {
      cypher.ORDER_BY(person, dept).toQuery(context) shouldBe DSLResult("ORDER BY a0,a1")
    }
    "return elements for a property" in {
      cypher.ORDER_BY(person('name), dept).toQuery(context) shouldBe DSLResult("ORDER BY a0.name,a1")
    }
    "return elements for multiple properties" in {
      cypher.ORDER_BY(person('name, 'age), dept('name)).toQuery(context) shouldBe DSLResult(
        "ORDER BY a0.name,a0.age,a1.name")
    }
  }
  "ORDER_BY_DESC" should {
    val context = new Context()
    context.add(person)
    context.add(dept)

    "return query for an element in Context" in {
      cypher.ORDER_BY_DESC(person).toQuery(context) shouldBe DSLResult("ORDER BY a0 DESC")
    }

    "return empty statement if no elements passed" in {
      cypher.ORDER_BY_DESC().toQuery(context) shouldBe DSLResult.empty
    }

    "return query for more than one element in Context" in {
      cypher.ORDER_BY_DESC(person, dept).toQuery(context) shouldBe DSLResult("ORDER BY a0,a1 DESC")
    }
    "return elements for a property" in {
      cypher.ORDER_BY_DESC(person('name), dept).toQuery(context) shouldBe DSLResult("ORDER BY a0.name,a1 DESC")
    }
    "return elements for multiple properties" in {
      cypher.ORDER_BY_DESC(person('name, 'age), dept('name)).toQuery(context) shouldBe DSLResult(
        "ORDER BY a0.name,a0.age,a1.name DESC")
    }
  }
  "anyNode" should {
    "provide right query when not in context" in {
      cypher.MATCH(anyNode).toQuery() shouldBe DSLResult("MATCH (a0)")
    }
    "provide right query when in context" in {
      val ctx   = new Context()
      val anyN  = anyNode
      val anyN2 = anyNode
      cypher.MATCH(anyN).toQuery(ctx) shouldBe DSLResult("MATCH (a0)")
      cypher.MATCH(anyN).toQuery(ctx) shouldBe DSLResult("MATCH (a0)")
      cypher.MATCH(anyN2).toQuery(ctx) shouldBe DSLResult("MATCH (a1)")
    }
  }
  "anyRelation" should {
    "provide right query when not in context" in {
      cypher.MATCH(anyNode).toQuery() shouldBe DSLResult("MATCH (a0)")
    }
    "provide right query when in context" in {
      val ctx   = new Context()
      val anyN  = anyNode
      val anyN2 = anyNode
      val anyR  = anyRelation
      val anyR2 = anyRelation
      cypher.MATCH(anyN -| anyR |-> anyN2).toQuery(ctx) shouldBe DSLResult("MATCH (a0)-[a1]->(a2)")
      cypher.MATCH(anyN -| anyR |-> anyN2).toQuery(ctx) shouldBe DSLResult("MATCH (a0)-[a1]->(a2)")
      cypher.MATCH(anyN -| anyR2 |-> anyN2).toQuery(ctx) shouldBe DSLResult("MATCH (a0)-[a3]->(a2)")
    }
  }
}
