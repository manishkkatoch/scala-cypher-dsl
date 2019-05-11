# scala-cypher-dsl [![Build Status](https://travis-ci.org/manishkkatoch/scala-cypher-dsl.svg?branch=master)](https://travis-ci.org/manishkkatoch/scala-cypher-dsl)

A type safe DSL for writing Cypher Query Language in Scala.

###### Samples
```scala
//sample domain models
case class Person(id: String, name: String, age: Int)
case class WorksIn(sinceDays: Int)
case class IsFriendOf(since: Int, lastConnectedOn: String)
case class Department(id: String, name: String)
```

###### Basic Queries

```scala
import com.agrim.scala.cypherDSL.syntax._
//val person:Person
val query = cypher
    .MATCH(person)
    .RETURN(person)

query.toQuery(new Context())
//MATCH (a0:Person {id:{a0_id}, name: {a0_name}, age: {a0_age}})
//RETURN a0

val query = cypher
    .MATCH(person)
    .MATCH(person -| worksIn |-> department)
    .RETURN(person -> "worker", department -> dept)

query.toQuery(new Context())
//MATCH (a0:Person {id:{a0_id}, name: {a0_name}, age: {a0_age}})
//MATCH (a0)-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id}, name: {a2_name}})
//RETURN a0 as worker, a2 as dept
```