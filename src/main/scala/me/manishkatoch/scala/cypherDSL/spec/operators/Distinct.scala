package me.manishkatoch.scala.cypherDSL.spec.operators
import me.manishkatoch.scala.cypherDSL.spec.Context
import me.manishkatoch.scala.cypherDSL.spec.entities.AliasedProduct
import me.manishkatoch.scala.cypherDSL.spec.utils.ElementPropertyExtractingAndAliasing

private[spec] case class Distinct(element: AliasedProduct) extends Operator with ElementPropertyExtractingAndAliasing {
  override def toQuery(context: Context): String = {
    val (el, properties) = getElementAndProperties(element.node)
    val ids = context
      .get(el)
      .map(identifier => {
        if (element.alias.isDefined) context.update(element.node, element.alias.get)
        makeAliasedString(identifier, properties, element.alias)
      })
      .getOrElse(throw new NoSuchElementException("DISTINCT operator requires element to be in Context"))

    if (ids.nonEmpty) s"DISTINCT $ids" else ""
  }
}
