# scala-cypher-dsl [![Build Status](https://travis-ci.org/manishkkatoch/scala-cypher-dsl.svg?branch=master)](https://travis-ci.org/manishkkatoch/scala-cypher-dsl) ![Sonatype Nexus (Releases)](https://img.shields.io/nexus/r/https/oss.sonatype.org/me.manishkatoch/scala-cypher-dsl.svg) ![Maven Central](https://img.shields.io/maven-central/v/me.manishkatoch/scala-cypher-dsl.svg)

A type-safe, compile time DSL for writing Cypher queries in Scala.

## Motivation

With Neo4J and Scala, the ORMs satisfy only a small subset of querying needs and majority of the fairly complex cypher query tend to be in the form of strings.Cypher strings (like SQL strings) have inherent issues like no type safety, minimal syntax checking, difficulty in composing etc.

Scala-Cypher-DSL aims to alleviate above by providing following:
1. Type-safe constructs using user defined models and ADTs
2. Chainable DSL like Cypher.
3. Automatic identifiers generation( you don't have to manage identifiers, just work with instances/models).
4. Parameterized queries and automatic creation of parameters map. 

Note: _It does not provide drivers for Neo4J but only concerns with query and query parameters creation_

## Installation

Binary release artefacts are published to the Sonatype OSS Repository Hosting service and synced to Maven Central.

#### SBT
```sbt
"me.manishkatoch" %% "scala-cypher-dsl" % "0.4.2"
```
#### Gradle
```gradle
implementation group: 'me.manishkatoch', name: 'scala-cypher-dsl', version: '0.4.2'
```

## Usage

Consider following domain models representing people working in a fictitious department and friendly by nature. 
```scala
//sample domain models
case class Person(id: String, name: String, age: Int)
case class WorksIn(sinceDays: Int)
case class IsFriendOf(since: Int, lastConnectedOn: String)
case class Department(id: String, name: String)
```
To start writing query DSL, import the following
```scala
import me.manishkatoch.scala.cypherDSL.spec.syntax.v1._
import me.manishkatoch.scala.cypherDSL.spec.syntax.patterns._ //optional, import for expressing paths.
```

using DSL for a simple match query generation for an instance of model
```scala
//for a person John Doe
val johnDoe = Person("AX31SD", "John Doe", 50)

//match and return Neo4J data
val johnDoeQuery = cypher.MATCH(johnDoe)
    .RETURN(johnDoe)
    .toQuery()

johnDoeQuery.query
//res0: String = MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})
//              RETURN a0

johnDoeQuery.queryMap
//res1: scala.collection.immutable.Map[String,Any] = Map(a0_id -> AX31SD, a0_name -> John Doe, a0_age -> 50))
```
using DSL for matching any instance of model.
```scala
//for any person
val anyPerson = any[Person] // any instance of node labelled Person

val result = cypher.MATCH(anyPerson)
    .RETURN(anyPerson)
    .toQuery()

result.query
//res0: String = MATCH (a0:Person)
//               RETURN a0

result.queryMap
//res1: scala.collection.immutable.Map[String,Any] = Map()
```
query for all the friends of John Doe in Science department
```scala
val scienceDept = Department("ZSW12R", "Science")
val anyPerson = any[Person]
val isFriendOf = anyRel[IsFriendOf] //any relation instance of label IsFriendOf

val result = cypher.MATCH(johnDoe -| isFriendOf |-> anyPerson <-- scienceDept)
    .RETURN(anyPerson)
    .toQuery()

result.query
//res0: String = MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:IS_FRIEND_OF]->(a2:Person)<--(a3:Department {id: {a3_id},name: {a3_name}})
//               RETURN a2

result.queryMap
//res1: scala.collection.immutable.Map[String,Any] = Map(a0_id -> AX31SD, a0_name -> John Doe, a3_name -> Science, a0_age -> 50, a3_id -> ZSW12R)
```
for detailed DSL usage and more examples, see [Wiki](https://github.com/manishkkatoch/scala-cypher-dsl/wiki)

## DSL Specifications

as of v0.4.2

| Cypher Clauses | DSL Support |
|----------------|-------------|
| MATCH | :white_check_mark: |
| OPTIONAL MATCH | :white_check_mark: |
| START | :x: |
| RETURN | :white_check_mark: |
| WITH | :white_check_mark: |
| UNWIND | :x: |
| WHERE | :x: |
| ORDER BY | :white_check_mark: |
| SKIP | :white_check_mark: |
| LIMIT | :white_check_mark: |
| CREATE | :white_check_mark: |
| DELETE | :white_check_mark: |
| SET | :white_check_mark: |
| REMOVE | :x: |
| FOREACH | :x: |
| MERGE | :white_check_mark: |
| CALL […​YIELD]| :x: |
| CREATE UNIQUE | :x: |
| UNION | :x: |

## Contributors and Participation
scala-cypher-dsl is currently maintained by [Manish Katoch](https://github.com/manishkkatoch/).

Any form of contribution (issue report, PR, etc) is more than welcome.

## Special Mentions
This project is made possible by [Shapeless](https://github.com/milessabin/shapeless). Special thanks to [Miles Sabin](https://github.com/milessabin)


