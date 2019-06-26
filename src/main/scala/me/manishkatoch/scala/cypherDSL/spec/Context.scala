package me.manishkatoch.scala.cypherDSL.spec

import scala.collection.mutable

/** Context manages state of a [[Statement]] */
class Context {
  private val objects: mutable.Map[Any, String] = mutable.Map.empty
  private val identifier                        = "a"

  /** adds an instance to the context and returns the id created
    * the id generated is @identifier + (length of objects stored till now + 1)
    * @param element instance to be added of type [[T]]
    * @tparam T
    * @return [[String]] identifier
    */
  def add[T](element: T): String = {
    val id = s"$identifier${objects.toSeq.length}"
    objects += element -> id
    id
  }

  /** updates the [[Context]] for the given element by creating new identifier
    * @param element instance for which context needs to be updated
    * @param id new identifier [[String]]
    * @tparam T type of the instance
    * @return the new identifier [[String]]
    */
  def update[T](element: T, id: String): String = {
    objects += element -> id
    id
  }

  def get[T](element: T): Option[String]               = objects.get(element)
  def map[T, B](element: T)(f: String => B): Option[B] = objects.get(element).map(f)

}
