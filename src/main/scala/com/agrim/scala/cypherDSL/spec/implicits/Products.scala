package org.agrimaa.scalacql.spec
import org.agrimaa.scalacql.spec.implicits.{CypherProduct, QueryProvider}
import shapeless.{HList, HNil}

private[spec] object Products {
  implicit class RichProduct[T <: Product](element: T)(implicit context: Context) {
    def apply(implicit
              context: Context,
              queryProvider: QueryProvider[T]): CypherProduct[T, _ <: HList] = {
      CypherProduct(element, HNil)
    }
    def apply(name: Symbol)(implicit
                            context: Context,
                            queryProvider: QueryProvider[T]): CypherProduct[T, _ <: HList] = {
      CypherProduct(element, name :: HNil)
    }
    def apply(sym1: Symbol, sym2: Symbol)(implicit
                                          context: Context,
                                          queryProvider: QueryProvider[T]): CypherProduct[T, _ <: HList] =
      CypherProduct(element, sym1 :: sym2 :: HNil)

    def apply(sym1: Symbol, sym2: Symbol, sym3: Symbol)(implicit
                                                        context: Context,
                                                        queryProvider: QueryProvider[T]): CypherProduct[T, _ <: HList] =
      CypherProduct(element, sym1 :: sym2 :: sym3 :: HNil)

    def apply(sym1: Symbol, sym2: Symbol, sym3: Symbol, sym4: Symbol)(
        implicit
        context: Context,
        queryProvider: QueryProvider[T]): CypherProduct[T, _ <: HList] =
      CypherProduct(element, sym1 :: sym2 :: sym3 :: sym4 :: HNil)

    def apply(sym1: Symbol, sym2: Symbol, sym3: Symbol, sym4: Symbol, sym5: Symbol)(
        implicit
        context: Context,
        queryProvider: QueryProvider[T]): CypherProduct[T, _ <: HList] =
      CypherProduct(element, sym1 :: sym2 :: sym3 :: sym4 :: sym5 :: HNil)

    def apply(sym1: Symbol, sym2: Symbol, sym3: Symbol, sym4: Symbol, sym5: Symbol, sym6: Symbol)(
        implicit
        context: Context,
        queryProvider: QueryProvider[T]): CypherProduct[T, _ <: HList] =
      new CypherProduct(element, sym1 :: sym2 :: sym3 :: sym4 :: sym5 :: sym6 :: HNil)

    def apply(sym1: Symbol, sym2: Symbol, sym3: Symbol, sym4: Symbol, sym5: Symbol, sym6: Symbol, sym7: Symbol)(
        implicit
        context: Context,
        queryProvider: QueryProvider[T]): CypherProduct[T, _ <: HList] =
      CypherProduct(element, sym1 :: sym2 :: sym3 :: sym4 :: sym5 :: sym6 :: sym7 :: HNil)

    def apply(sym1: Symbol,
              sym2: Symbol,
              sym3: Symbol,
              sym4: Symbol,
              sym5: Symbol,
              sym6: Symbol,
              sym7: Symbol,
              sym8: Symbol)(implicit
                            context: Context,
                            queryProvider: QueryProvider[T]): CypherProduct[T, _ <: HList] =
      CypherProduct(element, sym1 :: sym2 :: sym3 :: sym4 :: sym5 :: sym6 :: sym7 :: sym8 :: HNil)

    def apply(sym1: Symbol,
              sym2: Symbol,
              sym3: Symbol,
              sym4: Symbol,
              sym5: Symbol,
              sym6: Symbol,
              sym7: Symbol,
              sym8: Symbol,
              sym9: Symbol)(implicit
                            context: Context,
                            queryProvider: QueryProvider[T]): CypherProduct[T, _ <: HList] =
      CypherProduct(element, sym1 :: sym2 :: sym3 :: sym4 :: sym5 :: sym6 :: sym7 :: sym8 :: sym9 :: HNil)

    def apply(sym1: Symbol,
              sym2: Symbol,
              sym3: Symbol,
              sym4: Symbol,
              sym5: Symbol,
              sym6: Symbol,
              sym7: Symbol,
              sym8: Symbol,
              sym9: Symbol,
              sym10: Symbol)(implicit
                             context: Context,
                             queryProvider: QueryProvider[T]): CypherProduct[T, _ <: HList] =
      CypherProduct(element, sym1 :: sym2 :: sym3 :: sym4 :: sym5 :: sym6 :: sym7 :: sym8 :: sym9 :: sym10 :: HNil)
  }
}

//(a)-->(b)
//(a)--(b)
//(a)--[r]--(b)
//(person('id))-->(department)
