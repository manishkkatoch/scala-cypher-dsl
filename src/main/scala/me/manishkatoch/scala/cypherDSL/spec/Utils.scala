package me.manishkatoch.scala.cypherDSL.spec
import shapeless.{::, HList, HNil}

/** provides various utility methods to the cypherDSL */
private object Utils {

  /**
  * Implicitly adds toList to an HList
    * @param list an HList to be converted to [[List]]
    * @tparam H where H <: HList
    */
  implicit class ListableHList[H <: HList](list: H) {
    /**
    * returns a [[List]] from [[HList]]
      * @tparam T expected return type
      * @return [[List]] of [[T]]
      */
    def toList[T]: List[T] = {
      def lpToList(list: HList): List[T] = list match {
        case HNil => List.empty
        case s :: tail => s.asInstanceOf[T] +: lpToList(tail)
      }
      lpToList(list)
    }
  }
}
