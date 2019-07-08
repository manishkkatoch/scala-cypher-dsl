package me.manishkatoch.scala.cypherDSL.spec.clauses
import me.manishkatoch.scala.cypherDSL.spec.clauses.SetTypes.{EntitySet, ProductSet}
import me.manishkatoch.scala.cypherDSL.spec.entities.{CypherEntity, Node, Relationship}
import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult, QueryProvider}
import shapeless.{HList, HNil}
import shapeless.ops.hlist.ToTraversable

/**
  *
  *  Sets(person -> List(person('name) -> "jane", person('age) -> 25))
  *  Sets(person('name) -> "jane")
  *  Sets(person -> List.empty)
  *
  */
private[spec] object SetTypes {

  type ProductSet = (Product, EntitySet)
  type EntitySet  = Map[CypherEntity, Any]
}

private[spec] class SetSpec(private val either: Either[ProductSet, EntitySet]) {

  def toQuery(context: Context): DSLResult = {
    either.fold(
      productSet => {
        val (element, entitySet) = productSet
        val result               = resultFromEntitySet(entitySet, context)
        val id                   = context.get(element).get
        result.copy(query = s"$id = {${result.query.replace(s"$id.","")}}")
      },
      entitySet => resultFromEntitySet(entitySet, context)
    )
  }

  private def resultFromEntitySet(entitySet: Map[CypherEntity, Any], context: Context) = {
    if (entitySet.isEmpty) DSLResult.empty
    else
      entitySet.keySet
        .map(entity => {
          val result = entity.toSetterQuery(context)
          result.copy(queryMap = result.queryMap ++ Map(result.queryMap.head._1 -> entitySet(entity)))
        })
        .reduce(_ ++ (_, ","))
  }

}

private[spec] class Sets(private val setSpec: SetSpec) extends Clause {

  override def toQuery(context: Context = new Context()): DSLResult = {
    val result = setSpec.toQuery(context)
    result.copy(query = s"SET ${result.query}")
  }
}

private[spec] object Sets {
  def apply[T <: Product](element: T, set: List[(CypherEntity, Any)]): Sets =
    new Sets(new SetSpec(Left(element -> set.toMap)))
  def apply[T <: CypherEntity](set: (CypherEntity, Any)*): Sets = new Sets(new SetSpec(Right(set.toMap)))
}
