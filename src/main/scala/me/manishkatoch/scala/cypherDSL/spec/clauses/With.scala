package me.manishkatoch.scala.cypherDSL.spec.clauses

import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult}
import me.manishkatoch.scala.cypherDSL.spec.entities.{AliasedProduct, Node, Relationship}
import me.manishkatoch.scala.cypherDSL.spec.operators.Operator
import me.manishkatoch.scala.cypherDSL.spec.utils.ElementPropertyExtractingAndAliasing
import me.manishkatoch.scala.cypherDSL.spec.Utils._

private[cypherDSL] class With(elements: Either[AliasedProduct, Operator]*)
    extends Clause
    with ElementPropertyExtractingAndAliasing {
  private val errorMessage = "One or more of the elements to be returned are not in Context!"

  @throws[NoSuchElementException]
  def toQuery(context: Context = new Context()): DSLResult = {
    val ids = elements
      .map(element => {
        if (element.isRight) element.right.get.toQuery(context).query
        else {
          val aliasedProduct   = element.left.get
          val (el, properties) = getElementAndProperties(aliasedProduct.node)
          context
            .get(el)
            .map(identifier => {
              val hasProperties = (aliasedProduct.node match {
                case s: Node[_, _] => s.properties.toList.length
                case s: Relationship[_, _] => s.properties.toList.length
                case s => 0
              }) > 0
              if (aliasedProduct.alias.isDefined && !hasProperties) context.update(el, aliasedProduct.alias.get)
              else {
                context.add(aliasedProduct.node)
                if(aliasedProduct.alias.isDefined) context.update(aliasedProduct.node, aliasedProduct.alias.get)
              }
              makeAliasedString(identifier, properties, aliasedProduct.alias)
            })
            .getOrElse(throw new NoSuchElementException(errorMessage))
        }
      })
      .mkString(",")

    DSLResult(if (ids.nonEmpty) s"WITH $ids" else "")
  }
}
private[cypherDSL] object With {
  private def makeEitherList(products: List[Product]): List[Either[AliasedProduct, Operator]] = products match {
    case Nil                        => List.empty
    case (s: Operator) :: remaining => Right(s) +: makeEitherList(remaining)
    case s :: remaining             => Left(AliasedProduct.makeAliasedProduct(s)) +: makeEitherList(remaining)
  }
  def apply(elements: Product*): With = new With(makeEitherList(elements.toList): _*)
  val empty                           = With(Seq.empty: _*)
}
