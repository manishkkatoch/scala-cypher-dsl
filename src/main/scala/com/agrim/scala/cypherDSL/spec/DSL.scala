package com.agrim.scala.cypherDSL.spec
import com.agrim.scala.cypherDSL.spec.implicits.QueryProvider
import shapeless.{HList, HNil}
object DSL {

  case class PathLink[T <: Product, H <: HList](leftLink: Option[String],
                                                element: CypherEntity[T, H],
                                                rightLink: Option[String]) {
    def toQuery(implicit context: Context): String =
      leftLink.map(_.toString).mkString + element.toQuery + rightLink.map(_.toString).mkString
  }

  case class Path(pathLinks: PathLink[_, _ <: HList]*) {
    def toQuery(implicit context: Context): String =
      pathLinks.map(_.toQuery).mkString

    def |-[U <: Product, UH <: HList](rel: U)(implicit context: Context, queryProvider: QueryProvider[U]) = {
      Path(pathLinks :+ PathLink(Some("-"), Node(rel, HNil), None): _*)
    }
    def |->[U <: Product, UH <: HList](rel: U)(implicit context: Context, queryProvider: QueryProvider[U]) = {
      Path(pathLinks :+ PathLink(Some("->"), Node(rel, HNil), None): _*)
    }
  }

  case class RelPath(pathLinks: PathLink[_, _ <: HList]*) {
    def toQuery(implicit context: Context): String =
      pathLinks.map(_.toQuery).mkString

  }

//  implicit class RichPath(path: Path) {
//    def -[U <: Product, UH <: HList](rel: U)(implicit context: Context, queryProvider: QueryProvider[U]) = {
//      Path(path.pathLinks :+ PathLink(Some("-"), Node(rel, HNil), None): _*)
//    }
//    def ->[U <: Product, UH <: HList](rel: U)(implicit context: Context, queryProvider: QueryProvider[U]) = {
//      Path(path.pathLinks :+ PathLink(Some("->"), Node(rel, HNil), None): _*)
//    }
//  }
//  implicit class RichProduct[T <: Product](element: T) {
//    def -[U <: Product, UH <: HList](rel: U)(implicit queryProviderU: QueryProvider[U],
//                                             queryProvider: QueryProvider[T]) =
//      Path(PathLink(None, Node(element), Some(PathDirection.Undirected)), PathLink(None, rel, None))
//    def --[U <: Product](rel: U)(implicit queryProviderU: QueryProvider[U], queryProvider: QueryProvider[T]) =
//      Path(PathLink(None, element, Some(PathDirection.Undirected)), PathLink(Some(PathDirection.Undirected), rel, None))
//    def <--[U <: Product](rel: U)(implicit queryProviderU: QueryProvider[U], queryProvider: QueryProvider[T]) =
//      Path(PathLink(None, element, Some(PathDirection.Left)), PathLink(Some(PathDirection.Undirected), rel, None))
//    def -->[U <: Product](rel: U)(implicit queryProviderU: QueryProvider[U], queryProvider: QueryProvider[T]) =
//      Path(PathLink(None, element, Some(PathDirection.Undirected)), PathLink(Some(PathDirection.Right), rel, None))
//  }
}
