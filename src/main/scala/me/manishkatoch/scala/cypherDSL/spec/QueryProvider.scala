package me.manishkatoch.scala.cypherDSL.spec

import shapeless.ops.record.Keys
import shapeless.{HList, LabelledGeneric}

trait QueryProvider[T <: Product] {
  def getMatchers(element: T)(implicit context: Context): Seq[DSLResult]
  def getMatchers[U <: HList](element: T, columns: U)(implicit context: Context): Seq[DSLResult]
}

object QueryProvider {
  def apply[T <: Product](implicit queryProvider: QueryProvider[T]): QueryProvider[T] =
    queryProvider

  implicit def makeQueryProvider[T <: Product, H <: HList, K <: HList](
      implicit
      lGen: LabelledGeneric.Aux[T, H],
      keys: Keys.Aux[H, K]): QueryProvider[T] = new QueryProvider[T] {

      override def getMatchers(element: T)(implicit context: Context): Seq[DSLResult] = {
        val id = context.get(element).get
        val record = Utils.toList(keys()) zip Utils.toAnyList(lGen.to(element))
        recordToDSLResults(id, record)
      }

      override def getMatchers[U <: HList](element: T, columns: U)(implicit context: Context): Seq[DSLResult] = {
        val id = context.get(element).get
        val record = Utils.toList(keys()) zip Utils.toAnyList(lGen.to(element))
        val columnList = Utils.toList(columns)
        val filteredRecord = record.filter(t => columnList.contains(t._1))
        recordToDSLResults(id, filteredRecord)
      }

      private def recordToDSLResults(id: String, record: Seq[(String,Any)]):Seq[DSLResult] =
        record.map(tuple => {
          val (key, value) = tuple
          val queryString = Utils.matchPropertyPattern(id, key)
          val propertyPlaceHolder = Utils.propertyValuePlaceHolder(id, key)
          DSLResult(queryString, Map(propertyPlaceHolder -> value))
        })
    }

}
