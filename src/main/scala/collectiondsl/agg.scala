package collectiondsl

import cats._
import cats.implicits._


trait AggFunc[-A, +Out, Comb] with
  def empty: Comb

  def (comb: Comb) add (value: A): Comb
  def (comb1: Comb) combine (comb2: Comb): Comb

  def [O >: Out](comb: Comb) extract: AggFunc.Result[O, Comb]

object AggFunc with
  final case class Result[Out, Comb](result: Out, combiner: Comb)

  /**
  * An extendable sum type
  * representing some type of aggregations
  **/
  trait Type extends Serializable
  object Type with
    sealed trait Sum     extends Type
    sealed trait Count   extends Type
    sealed trait Avg     extends Type
    sealed trait Max     extends Type
    sealed trait Min     extends Type
    sealed trait Product extends Type
    sealed trait Random  extends Type

    /** Collect values into collection */
    sealed trait Arr extends Type

    /** Concatenate string representations of the values */
    sealed trait StringAgg extends Type

    /** Standard deviation */
    sealed trait STDEV extends Type

    /** Root mean square */
    sealed trait RMS extends Type


val sum: AggFunc.Type.Sum             = new AggFunc.Type.Sum       {}
val count: AggFunc.Type.Count         = new AggFunc.Type.Count     {}
val avg: AggFunc.Type.Avg             = new AggFunc.Type.Avg       {}
val max: AggFunc.Type.Max             = new AggFunc.Type.Max       {}
val min: AggFunc.Type.Min             = new AggFunc.Type.Min       {}
val product: AggFunc.Type.Product     = new AggFunc.Type.Product   {}
val random: AggFunc.Type.Random       = new AggFunc.Type.Random    {}
val arr: AggFunc.Type.Arr             = new AggFunc.Type.Arr       {}
val stringAgg: AggFunc.Type.StringAgg = new AggFunc.Type.StringAgg {}
val stdev: AggFunc.Type.STDEV         = new AggFunc.Type.STDEV     {}
val rms: AggFunc.Type.RMS             = new AggFunc.Type.RMS       {}
    
case class TaggedAgg[A, U, AggT <: AggFunc.Type](tagged: A :@ U)

case class QueryResult[Coll[+x] <: Seq[x], +A, K, +T](keys: K, totals: T, values: Coll[A])