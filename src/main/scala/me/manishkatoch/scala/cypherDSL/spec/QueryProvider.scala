package me.manishkatoch.scala.cypherDSL.spec

import shapeless.ops.record.Keys
import shapeless.{HList, LabelledGeneric}
import Utils._

/** trait [[QueryProvider]] provides methods to get matching properties and values for an instance of type [[T]]
  * @tparam T type for which the [[QueryProvider]] expects an instance
  */
trait QueryProvider[T <: Product] {
  /** returns a [[DSLResult]] for a given element and all its properties
    * @param element instance of type [[T]]
    * @param context the [[Context]] of the [[Statement]]
    * @return [[DSLResult]]
    */
  def getMatchers(element: T)(implicit context: Context): Seq[DSLResult]

  /** returns a [[DSLResult]] for a given element and selected properties
    * @param element instance of type [[T]]
    * @param columns an [[HList]] of [[Symbol]] corresponding to properties selected
    * @param context the [[Context]] of the [[Statement]] which holds the @param element
    * @tparam U type of columns where U <: [[HList]]
    * @return
    */
  def getMatchers[U <: HList](element: T, columns: U)(implicit context: Context): Seq[DSLResult]
  def getSetters(element: T)(implicit context: Context): Seq[DSLResult]

  def getSetters[U <: HList](element: T, columns: U)(implicit context: Context): Seq[DSLResult]

}

/** Factory for [[me.manishkatoch.scala.cypherDSL.spec.QueryProvider]] instances */
object QueryProvider {
  def apply[T <: Product](implicit queryProvider: QueryProvider[T]): QueryProvider[T] =
    queryProvider

  /** Default [[me.manishkatoch.scala.cypherDSL.spec.QueryProvider]] implementation
  * this is served implicitly whenever an implicit instance of [[QueryProvider]] is required but unavailable
    * @param lGen implicit [[LabelledGeneric]] to extract [[HList]] from case class.
    * @param keys implicit [[Keys]] extracted at compile time from @lGen
    * @tparam T type of element instance
    * @tparam H expected type of HList
    * @tparam K expected type of @keys
    * @return [[QueryProvider]] instance
    */
  implicit def makeQueryProvider[T <: Product, H <: HList, K <: HList](implicit
                                                                       lGen: LabelledGeneric.Aux[T, H],
                                                                       keys: Keys.Aux[H, K]): QueryProvider[T] =
    new QueryProvider[T] {

      override def getMatchers(element: T)(implicit context: Context): Seq[DSLResult] = {
        val id     = context.get(element).get
        val record = keys().toList[Symbol].map(_.name) zip lGen.to(element).toList[Any]
        recordToDSLResults(id, record)(matchPropertyPattern)
      }

      override def getMatchers[U <: HList](element: T, columns: U)(implicit context: Context): Seq[DSLResult] = {
        val id             = context.get(element).get
        val record         = keys().toList[Symbol].map(_.name) zip lGen.to(element).toList[Any]
        val columnList     = columns.toList[Symbol].map(_.name)
        val filteredRecord = record.filter(t => columnList.contains(t._1))
        recordToDSLResults(id, filteredRecord)(matchPropertyPattern)
      }

      override def getSetters(element: T)(implicit context: Context): Seq[DSLResult] = {
        val id     = context.get(element).get
        val record = keys().toList[Symbol].map(_.name) zip lGen.to(element).toList[Any]
        recordToDSLResults(id, record)(setPropertyPattern)
      }

      override def getSetters[U <: HList](element: T, columns: U)(implicit context: Context): Seq[DSLResult] = {
        val id             = context.get(element).get
        val record         = keys().toList[Symbol].map(_.name) zip lGen.to(element).toList[Any]
        val columnList     = columns.toList[Symbol].map(_.name)
        val filteredRecord = record.filter(t => columnList.contains(t._1))
        recordToDSLResults(id, filteredRecord)(setPropertyPattern)
      }

      private def recordToDSLResults(id: String, record: Seq[(String, Any)])(
          queryStringGenerator: (String,String) => String,
          mapper: (String, (String, Any)) => Map[String, Any] = defaultMapper
      ): Seq[DSLResult] =
        record.map(tuple => DSLResult(queryStringGenerator(id, tuple._1), mapper(id, tuple)))

      private def defaultMapper(id: String, keyValuePair: (String, Any)): Map[String, Any] = {
        val (key, value) = keyValuePair
        Map(propertyValuePlaceHolder(id, key) -> value)
      }

      private def matchPropertyPattern(identifier: String, propertyName: String) =
        s"$propertyName: {${propertyValuePlaceHolder(identifier, propertyName)}}"

      private def setPropertyPattern(identifier: String, propertyName: String) =
        s"$identifier.$propertyName = {${propertyValuePlaceHolder(identifier, propertyName)}}"

      private def propertyValuePlaceHolder(identifier: String, propertyName: String) = s"${identifier}_$propertyName"

    }

}
