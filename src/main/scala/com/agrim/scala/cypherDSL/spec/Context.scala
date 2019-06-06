package com.agrim.scala.cypherDSL.spec

import scala.collection.mutable

class Context {
  private val objects: mutable.Map[Any, String] = mutable.Map.empty
  private val identifier                        = "a"

  def add[T](element: T): String = {
    val id = s"$identifier${objects.toSeq.length}"
    objects += element -> id
    id
  }
  def update[T](element: T, id: String): String = {
    objects += element -> id
    id
  }
  def get[T](element: T): Option[String]               = objects.get(element)
  def map[T, B](element: T)(f: String => B): Option[B] = objects.get(element).map(f)

}
