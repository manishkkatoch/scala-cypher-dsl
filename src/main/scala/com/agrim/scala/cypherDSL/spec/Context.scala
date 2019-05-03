package com.agrim.scala.cypherDSL.spec

import scala.collection.mutable

class Context {

  private val objects: mutable.Map[Product, String] = mutable.Map.empty
  private val identifier                            = "a"

  def add[T <: Product](element: T): String = {
    val id = s"$identifier${objects.toSeq.length}"
    objects += element -> id
    id
  }
  def get[T <: Product](element: T): Option[String] = objects.get(element)
}

object Context {
  def apply: Context = new Context()
}
