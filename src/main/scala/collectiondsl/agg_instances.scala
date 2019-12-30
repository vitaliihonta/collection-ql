package collectiondsl

import scala.language.implicitConversions
import scala.compiletime.{erasedValue, error, summonFrom, code}
import cats.Monoid
import cats.syntax.all._
import AggFunc.Type
import scala.quoted._

given aggSum[A, U]: (given aMonoid: Monoid[A]) => AggFunc[TaggedAgg[A, U, Type.Sum], A :@ U, A] =
  new AggFunc[TaggedAgg[A, U, AggFunc.Type.Sum], A :@ U, A] with
    def empty: A = aMonoid.empty
    def (comb: A) add (value: TaggedAgg[A, U, AggFunc.Type.Sum]): A =
      comb |+| value.tagged.value
    def (c1: A) combine (c2: A): A = c1 |+| c2

    def [O >: A :@ U](comb: A) extract: AggFunc.Result[O, A] = 
      AggFunc.Result(comb.as[U], comb)
  
given taggedAggCount[A, U]: AggFunc[TaggedAgg[A, U, AggFunc.Type.Count], Long :@ U, Long] =
  new AggFunc[TaggedAgg[A, U, AggFunc.Type.Count], Long :@ U, Long] {
    def empty: Long = 0
    def (comb: Long) add (value: TaggedAgg[A, U, AggFunc.Type.Count]): Long =
      comb + 1
    def (c1: Long) combine (c2: Long): Long = c1 + c2

    def [O >: Long :@ U](comb: Long) extract: AggFunc.Result[O, Long] = 
      AggFunc.Result(comb.as[U], comb)
  }

type TupledAggFunc[A <: Tuple, Out <: Tuple, Comb] = AggFunc[A, Out, Comb]
inline given tupleAgg[L <: Tuple] <: TupledAggFunc[?, ?, ?] = inline erasedValue[L] match
  case _: Unit => new AggFunc[Unit, Unit, Unit] with
    override def empty = ()
    override def (c1: Unit) add (v: Unit) = ()
    override def (c1: Unit) combine (c2: Unit) = ()
    override def [O >: Unit] (c: Unit) extract: AggFunc.Result[O, Unit] = AggFunc.Result((), ())
  
  case sample: (TaggedAgg[a, u, aggF] *: t) => 
    summonFrom {
      case given headAgg: AggFunc[TaggedAgg[`a`, `u`, `aggF`], hOut, hComb] =>
        inline tupleAgg[t] match
          case tailAgg: TupledAggFunc[`t`, tOut, tComb] =>
            given AggFunc[t, tOut, tComb] = tailAgg
            new AggFunc[TaggedAgg[a, u, aggF] *: t, hOut *: tOut, hComb *: tComb *: Unit] with

              override def empty: hComb *: tComb *: Unit = 
                headAgg.empty *: tailAgg.empty *: ()

              override def (c1: hComb *: tComb *: Unit) add (v: TaggedAgg[a, u, aggF] *: t) =
                val (h1, t1) = c1
                val (h2 *: t2) = v
                h1.add(h2) *: t1.add(t2) *: ()

              override def (comb1: hComb *: tComb *: Unit) combine (comb2: hComb *: tComb *: Unit): hComb *: tComb *: Unit = 
                val (h1, t1) = comb1
                val (h2, t2) = comb2
                
                (h1.combine(h2), t1.combine(t2))
                
              override def [O >: hOut *: tOut] (comb: hComb *: tComb *: Unit) extract: AggFunc.Result[O, hComb *: tComb *: Unit] = 
                val (head, tail) = comb
                val headRes = head.extract
                val tailRes = tail.extract
                AggFunc.Result(headRes.result *: tailRes.result , comb)

      case _ => error(code"No instance of AggFunc given for TaggedAgg $sample")
    }
    
    