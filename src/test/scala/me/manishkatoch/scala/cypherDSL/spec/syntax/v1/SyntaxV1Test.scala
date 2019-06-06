package me.manishkatoch.scala.cypherDSL.spec.syntax.v1

import me.manishkatoch.scala.cypherDSL.spec.Context
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
      cypher
        .MATCH(person)
        .toQuery(new Context()) shouldBe "MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})"
    }
    "provide query for a node" in {
      cypher.MATCH(person('name)).toQuery(new Context()) shouldBe "MATCH (a0:Person {name: {a0_name}})"
    }
    "provide query for a class" in {
      cypher.MATCH(anyPerson).toQuery(new Context()) shouldBe "MATCH (a0:Person)"
    }
    "provide query for a path" in {
      cypher
        .MATCH(person -| worksIn |-> dept)
        .toQuery(new Context()) shouldBe "MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})"
    }
  }
  "OPTIONAL_MATCH" should {
    "provide query for an instance" in {
      cypher
        .OPTIONAL_MATCH(person)
        .toQuery(new Context()) shouldBe "OPTIONAL MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})"
    }
    "provide query for a node" in {
      cypher
        .OPTIONAL_MATCH(person('name))
        .toQuery(new Context()) shouldBe "OPTIONAL MATCH (a0:Person {name: {a0_name}})"
    }
    "provide query for a class" in {
      cypher.OPTIONAL_MATCH(any[Person]).toQuery(new Context()) shouldBe "OPTIONAL MATCH (a0:Person)"
    }
    "provide query for a path" in {
      cypher
        .OPTIONAL_MATCH(person -| worksIn |-> dept)
        .toQuery(new Context()) shouldBe "OPTIONAL MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})"
    }
  }
  "SKIP" should {
    "provide query for a given count" in {
      cypher.SKIP(10).toQuery(new Context()) shouldBe "SKIP 10"
    }
  }
  "LIMIT" should {
    "provide query for a given count" in {
      cypher.LIMIT(10).toQuery(new Context()) shouldBe "LIMIT 10"
    }
  }
  "RETURN" should {
    val context = new Context()
    context.add(person)
    context.add(anyPerson)
    context.add(dept)
    "return query for an element in Context" in {
      cypher.RETURN(person).toQuery(context) shouldBe "RETURN a0"
    }
    "return query for any element in Context" in {
      cypher.RETURN(anyPerson).toQuery(context) shouldBe "RETURN a1"
    }
    "return empty statement if no elements passed" in {
      cypher.RETURN().toQuery(context) shouldBe ""
    }
    "return query for more than one element in Context" in {
      cypher.RETURN(person, anyPerson).toQuery(context) shouldBe "RETURN a0,a1"
    }
    "return elements for a property" in {
      cypher.RETURN(person('name), dept).toQuery(context) shouldBe "RETURN a0.name,a2"
    }
    "return elements for multiple properties" in {
      cypher.RETURN(person('name, 'age), dept('name)).toQuery(context) shouldBe "RETURN a0.name,a0.age,a2.name"
    }
    "throw if element to be returned not in Context" in {
      the[NoSuchElementException] thrownBy {
        cypher.RETURN(worksIn).toQuery(context)
      } should have message "One or more of the elements to be returned are not in Context!"
    }
    "return aliased elements" in {
      cypher
        .RETURN(person -> "person", dept -> "department")
        .toQuery(context) shouldBe "RETURN a0 as person,a2 as department"
    }
    "return non-aliased and aliased elements in a single return" in {
      cypher.RETURN(person, dept -> "department").toQuery(context) shouldBe "RETURN a0,a2 as department"
    }
    "return aliased elements for a property" in {
      cypher
        .RETURN(person('name) -> "personName", dept -> "department")
        .toQuery(context) shouldBe "RETURN a0.name as personName,a2 as department"
    }
    "return aliased elements for multiple properties" in {
      cypher
        .RETURN(person('name) -> "name", person('age) -> "age", dept('name) -> "departmentName")
        .toQuery(context) shouldBe "RETURN a0.name as name,a0.age as age,a2.name as departmentName"
    }
  }
  "WITH" should {
    val context = new Context()
    context.add(person)
    context.add(dept)
    "WITH query for an element in Context" in {
      cypher.WITH(person).toQuery(context) shouldBe "WITH a0"
    }

    "WITH empty statement if no elements passed" in {
      cypher.WITH().toQuery(context) shouldBe ""
    }

    "WITH query for more than one element in Context" in {
      cypher.WITH(person, dept).toQuery(context) shouldBe "WITH a0,a1"
    }
    "WITH elements for a property" in {
      cypher.WITH(person('name), dept).toQuery(context) shouldBe "WITH a0.name,a1"
    }
    "WITH elements for multiple properties" in {
      cypher.WITH(person('name, 'age), dept('name)).toQuery(context) shouldBe "WITH a0.name,a0.age,a1.name"
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
      cypher.WITH(person -> "person", dept -> "dept").toQuery(context) shouldBe "WITH a0 as person,a1 as dept"
    }
    "WITH non-aliased and aliased elements in a single WITH" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher.WITH(person, dept -> "dept").toQuery(context) shouldBe "WITH a0,a1 as dept"
    }
    "WITH aliased elements for a property" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher
        .WITH(person('name) -> "personName", dept -> "dept")
        .toQuery(context) shouldBe "WITH a0.name as personName,a1 as dept"
    }
    "WITH aliased elements for multiple properties" in {
      cypher
        .WITH(person('name) -> "name", person('age) -> "age", dept('name) -> "departmentName")
        .toQuery(context) shouldBe "WITH a0.name as name,a0.age as age,a1.name as departmentName"
    }
    "WITH aliased elements should return alias going forward" in {
      cypher.WITH(person -> "p").toQuery(context)
      cypher.RETURN(person).toQuery(context) shouldBe "RETURN p"
    }
  }
  "ORDER_BY" should {
    val context = new Context()
    context.add(person)
    context.add(dept)

    "return query for an element in Context" in {
      cypher.ORDER_BY(person).toQuery(context) shouldBe "ORDER BY a0"
    }

    "return empty statement if no elements passed" in {
      cypher.ORDER_BY().toQuery(context) shouldBe ""
    }

    "return query for more than one element in Context" in {
      cypher.ORDER_BY(person, dept).toQuery(context) shouldBe "ORDER BY a0,a1"
    }
    "return elements for a property" in {
      cypher.ORDER_BY(person('name), dept).toQuery(context) shouldBe "ORDER BY a0.name,a1"
    }
    "return elements for multiple properties" in {
      cypher.ORDER_BY(person('name, 'age), dept('name)).toQuery(context) shouldBe "ORDER BY a0.name,a0.age,a1.name"
    }
  }
  "ORDER_BY_DESC" should {
    val context = new Context()
    context.add(person)
    context.add(dept)

    "return query for an element in Context" in {
      cypher.ORDER_BY_DESC(person).toQuery(context) shouldBe "ORDER BY a0 DESC"
    }

    "return empty statement if no elements passed" in {
      cypher.ORDER_BY_DESC().toQuery(context) shouldBe ""
    }

    "return query for more than one element in Context" in {
      cypher.ORDER_BY_DESC(person, dept).toQuery(context) shouldBe "ORDER BY a0,a1 DESC"
    }
    "return elements for a property" in {
      cypher.ORDER_BY_DESC(person('name), dept).toQuery(context) shouldBe "ORDER BY a0.name,a1 DESC"
    }
    "return elements for multiple properties" in {
      cypher
        .ORDER_BY_DESC(person('name, 'age), dept('name))
        .toQuery(context) shouldBe "ORDER BY a0.name,a0.age,a1.name DESC"
    }
  }
}
