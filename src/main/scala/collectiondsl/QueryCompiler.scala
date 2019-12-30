package collectiondsl

import scala.collection.{BuildFrom, View}


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
  def compile: Seq[QueryResult[Seq, A, G, R]] =
    type QueryRes = QueryResult[Vector, A, G, R]

    val builder = Vector.newBuilder[A]
    val grouped: Seq[QueryRes] = pipeline
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
          val maybeSorted = orderRecords.fold(asCol) { (ord) =>
            given Ordering[A] = ord
            asCol.sorted
          }
          aggRes -> maybeSorted
      }
      .map {
        case (g, (aggRes, vs)) => QueryResult(g, aggRes, vs)
      }
      .filter(qr => havingF(qr.totals))
      .toSeq

      val orderedByR: Seq[QueryRes] = 
        orderResults.fold(grouped) { ord =>
          given Ordering[R] = ord
          grouped.sortBy(_.totals)
        }

      val orderedByG: Seq[QueryRes] = 
        orderCriterias.fold(orderedByR) { ord =>
          given Ordering[G] = ord
          orderedByR.sortBy(_.keys)
        }

      orderedByG

  end compile
end Query