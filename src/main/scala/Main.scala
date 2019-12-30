import scala.language.implicitConversions
import collectiondsl.{startQl, tupleAgg, _}
import collectiondsl.{ given AggFunc[?, ?, ?] }
import cats.implicits.{ given cats.Monoid[?] }

@main def foo =
  val numbers = 1L to 20L
  val result = numbers.to(Vector)
    .where(_ > 5)
    .groupBy(
      expr[Long](_ % 2 == 0) as "divisible by 2",
      expr[Long](_ % 3 == 0) as "divisible by 3",
      expr[Long](_ % 4) as "reminder of 4"
    )
    // .aggregate(
    //   col[Long] agg count as "count",
    //   expr[Long](num => num * num * num * num) agg sum as "^4",
    //   expr[Long](_.toString) agg sum as "some name"
    // )

  type T = TaggedAgg[Int, "count", AggFunc.Type.Sum] *: Unit
  // summon[AggFunc[TaggedAgg[Int, "count", AggFunc.Type.Count]]]
  val foo = tupleAgg[T]
  
  // foo

