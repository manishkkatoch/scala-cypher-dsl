package me.manishkatoch.scala.cypherDSL.spec.clauses

import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult}
import me.manishkatoch.scala.cypherDSL.spec.entities.{AliasedProduct, CypherType}
import me.manishkatoch.scala.cypherDSL.spec.operators.Operator
import me.manishkatoch.scala.cypherDSL.spec.utils.ElementPropertyExtractingAndAliasing

private[cypherDSL] class Returns(elements: Either[AliasedProduct, Operator]*)
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
          context.get(aliasedProduct.node)
            .map(id => (id, List.empty[String]))
            .orElse {
              val (el, properties) = getElementAndProperties(aliasedProduct.node)
              Option((context.get(el).getOrElse(throw new NoSuchElementException(errorMessage)), properties))
            }
            .map(s => makeAliasedString(s._1, s._2, aliasedProduct.alias))
            .getOrElse(throw new NoSuchElementException(errorMessage))
        }
      })
      .mkString(",")

    DSLResult(if (ids.nonEmpty) s"RETURN $ids" else "")
  }
}
private[cypherDSL] object Returns {
  private def makeEitherList(products: List[Product]): List[Either[AliasedProduct, Operator]] = products match {
    case Nil                        => List.empty
    case (s: Operator) :: remaining => Right(s) +: makeEitherList(remaining)
    case s :: remaining             => Left(AliasedProduct.makeAliasedProduct(s)) +: makeEitherList(remaining)
  }
  def apply(elements: Product*): Returns = {
    new Returns(makeEitherList(elements.toList): _*)
  }
  val empty = Returns(Seq.empty: _*)
}
