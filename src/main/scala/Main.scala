import scala.language.implicitConversions
import collectiondsl.{startQl, consAgg, converter, unitAgg, _}
import collectiondsl.{ given AggFunc[?, ?, ?], given QueryToCaseClass[?, ?, ?, ?, ?] }
import cats.implicits.{ given cats.Monoid[?] }

@main def foo =
  val numbers = (1L to 20L).to(List)
  val result = numbers
    .where(_ > 5)
    .groupBy(
      expr[Long](_ % 2 == 0) as "divisible by 2",
      expr[Long](_ % 3 == 0) as "divisible by 3",
      expr[Long](_ % 4) as "reminder of 4"
    )
    .aggregate(
      col[Long] agg count as "count",
      expr[Long](num => num * num * num * num) agg sum as "^4",
      expr[Long](_.toString) agg sum as "some name"
    )
    .having(_.get["some name"].contains('1'))
    .to(List)
     
    
  val report = result.as[NumbersReport]
  // val report2 = result.as[Foo0] // doesn't compile
  println(
    report.mkString("\n")
  )

case class Foo0(x: Int)
case class NumbersReport(
  divisibleBy2: Boolean,
  divisibleBy3: Boolean,
  reminderOf4: Long,
  count: Long,
  power4: Long,
  someName: String,
  values: List[Long])