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

given unitAgg: AggFunc[Unit, Unit, Unit] = new AggFunc[Unit, Unit, Unit] with
  override def empty = ()
  override def (c1: Unit) add (v: Unit) = ()
  override def (c1: Unit) combine (c2: Unit) = ()
  override def [O >: Unit] (c: Unit) extract: AggFunc.Result[O, Unit] = AggFunc.Result((), ())


given consAgg[A, U, AggF <: AggFunc.Type, HOut, HComb, T <: Tuple, TOut <: Tuple, TComb]: 
  (given headAgg: AggFunc[TaggedAgg[A, U, AggF], HOut, HComb],
  tailAgg: AggFunc[T, TOut, TComb]) => AggFunc[TaggedAgg[A, U, AggF] *: T, HOut *: TOut, (HComb, TComb)] = 
  new AggFunc[TaggedAgg[A, U, AggF] *: T, HOut *: TOut, (HComb, TComb)] with

    override def empty: (HComb, TComb) = 
      (headAgg.empty, tailAgg.empty)

    override def (c1: (HComb, TComb)) add (value: TaggedAgg[A, U, AggF] *: T) =
      val (h1, t1) = c1
      val h2 = value.head
      val t2 = value.tail
      (h1.add(h2), t1.add(t2))

    override def (comb1: (HComb, TComb)) combine (comb2: (HComb, TComb)): (HComb, TComb) = 
      val (h1, t1) = comb1
      val (h2, t2) = comb2
      
      (h1.combine(h2), t1.combine(t2))
      
    override def [O >: HOut *: TOut] (comb: (HComb, TComb)) extract: AggFunc.Result[O, (HComb, TComb)] = 
      val (head, tail) = comb
      val headRes = head.extract
      val tailRes = tail.extract
      AggFunc.Result(headRes.result *: tailRes.result , comb)  
  