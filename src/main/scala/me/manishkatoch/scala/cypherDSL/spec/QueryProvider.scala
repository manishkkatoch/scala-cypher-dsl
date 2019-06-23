package me.manishkatoch.scala.cypherDSL.spec

import shapeless.ops.hlist.ToTraversable
import shapeless.ops.record.Keys
import shapeless.{HList, LabelledGeneric}

trait QueryProvider[T <: Product] {
  def getMatchers(element: T)(implicit context: Context): Seq[String]
  def getMatchers[U <: HList](element: T, columns: U)(implicit context: Context,
                                                      i0: ToTraversable.Aux[U, List, Symbol]): Seq[String]
}

object QueryProvider {
  def apply[T <: Product](implicit queryProvider: QueryProvider[T]): QueryProvider[T] =
    queryProvider

  implicit def makeQueryProvider[T <: Product, H <: HList, K <: HList](
      implicit
      lGen: LabelledGeneric.Aux[T, H],
      keys: Keys.Aux[H, K],
      i0: ToTraversable.Aux[K, List, Symbol]): QueryProvider[T] =
    new QueryProvider[T] {

      override def getMatchers(element: T)(implicit context: Context): Seq[String] = {
        val id = context.get(element).get
        keys().toList.map(sym => Utils.matchPropertyPattern(id, sym.name))
      }

      override def getMatchers[U <: HList](element: T, columns: U)(
          implicit context: Context,
          i0: ToTraversable.Aux[U, List, Symbol]): Seq[String] = {
        val id = context.get(element).get
        columns.toList[Symbol].map(sym => Utils.matchPropertyPattern(id, sym.name))
      }
    }
}
