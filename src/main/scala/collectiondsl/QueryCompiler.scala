package collectiondsl

import scala.collection.{BuildFrom, Factory, View}


final case class Query[Coll[x] <: Seq[x], A, G <: Tuple, T <: Tuple, R <: Tuple, Comb](
  pipeline: Coll[A],
  getG: A => G,
  getT: A => T,
  filterF: A => Boolean,
  havingF: R => Boolean,
  orderRecords: Option[Ordering[A]],
  orderCriterias: Option[Ordering[G]],
  orderResults: Option[Ordering[R]]
)(given aggF: AggFunc[T, R, Comb]) with
  def compile(
    given buildFrom1: BuildFrom[Coll[A], A, Coll[A]],
    Coll: Factory[QueryResult[Seq, A, G, R], Coll[QueryResult[Coll, A, G, R]]],
    Coll2: Factory[QueryResult[Coll, A, G, R], Coll[QueryResult[Coll, A, G, R]]]
  ): Coll[QueryResult[Coll, A, G, R]] =
    type QueryRes = QueryResult[Coll, A, G, R]

    val builder = buildFrom1.newBuilder(pipeline)
    val grouped: Coll[QueryRes] = pipeline
      .filter(filterF)
      .groupBy(getG)
      .view
      .mapValues {
        (as) =>
          val totals = 
            as.foldLeft(aggF.empty) { 
              (comb, a) =>
                builder.addOne(a)
                comb.add(getT(a))
            }
          val asCol = builder.result()
          val aggRes: R = totals.extract.result
          val maybeSorted: Coll[A] = orderRecords.fold(asCol) { (ord) =>
            given Ordering[A] = ord
            val builder = buildFrom1.newBuilder(asCol)
            builder.addAll(asCol.sorted)
            builder.result()
          }
          aggRes -> maybeSorted
      }
      .map {
        case (g, (aggRes, vs)) => QueryResult(g, aggRes, vs)
      }
      .filter(qr => havingF(qr.totals))
      .to(Coll)

      val orderedByR: Coll[QueryRes] = 
        orderResults.fold(grouped) { ord =>
          given Ordering[R] = ord
          grouped.sortBy(_.totals).to(Coll2)
        }

      val orderedByG: Coll[QueryRes] = 
        orderCriterias.fold(orderedByR) { ord =>
          given Ordering[G] = ord
          orderedByR.sortBy(_.keys).to(Coll2)
        }

      orderedByG

  end compile
end Query