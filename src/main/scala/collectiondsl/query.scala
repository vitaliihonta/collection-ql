package collectiondsl

import scala.collection.{BuildFrom, Factory}

sealed trait QueryBuilder[Coll[x] <: Seq[x], A] with
  def pipeline: Coll[A]

object QueryBuilder with
  class Empty[Coll[x] <: Seq[x], A](override val pipeline: Coll[A]) 
    extends QueryBuilder[Coll, A]
    with groupBy22[Coll, A] with

     /**
      * Like Where clause in SQL
      *
      * @param p - predicate
      **/
    def where(p: A => Boolean): Where[Coll, A] = new Where(pipeline, p) 

  class Where[Coll[x] <: Seq[x], A](override val pipeline: Coll[A], val p: A => Boolean)
      extends QueryBuilder[Coll, A]
      with groupBy22[Coll, A] with

    def and(p2: A => Boolean): Where[Coll, A] =
      new Where(pipeline, (a: A) => p(a) && p2(a))
  
  
  class GroupBy[Coll[x] <: Seq[x], A, G <: Tuple](override val pipeline: Coll[A],
                                                  val getG: A => G,
                                                  val filterOpt: Option[Where[Coll, A]])
      extends QueryBuilder[Coll, A]
      with agg22[Coll, A, G]

  class Aggregate[Coll[x] <: Seq[x], A, G <: Tuple, T <: Tuple, R <: Tuple, Comb](
      override val pipeline: Coll[A],
      val getG: A => G,
      val getT: A => T,
      val filterOpt: Option[Where[Coll, A]]
  )(given aggF: AggFunc[T, R, Comb])
    extends QueryBuilder[Coll, A] with
  
      /**
        * Like Having clause in SQL
        *
        * @param p - predicate
        **/
      def having(p: R => Boolean): MaybeOrderedHaving[Coll, A, G, T, R, Comb] =
        new MaybeOrderedHaving(pipeline, getG, getT, p, filterOpt, None, None, None)
  
      /**
        * Like Order By clause in SQL
        * for record [[A]]
        *
        * @param Ord - implicit [[Ordering]] for [[A]]
        **/
      def orderRecords(
          implicit Ord: Ordering[A]
      ): MaybeOrderedHaving[Coll, A, G, T, R, Comb] =
        new MaybeOrderedHaving(
          pipeline,
          getG,
          getT,
          (_: R) => true,
          filterOpt,
          Some(Ord),
          None,
          None
        )
  
      /**
        * Like Order By clause in SQL
        * ordering records of type [[A]]
        * by some criteria [[B]]
        * having an implicit [[Ordering]] for [[B]]
        *
        * @param f - extract an ordering criteria
        **/
      def orderRecordsBy[B: Ordering](
          f: A => B
      ): MaybeOrderedHaving[Coll, A, G, T, R, Comb] = orderRecords(Ordering.by[A, B](f))
  
      /**
        * Like Order By clause in SQL
        * for grouping criteria [[G]]
        *
        * @param Ord - implicit [[Ordering]] for [[G]]
        **/
      def orderGroups(
          implicit Ord: Ordering[G]
      ): MaybeOrderedHaving[Coll, A, G, T, R, Comb] =
        new MaybeOrderedHaving(
          pipeline,
          getG,
          getT,
          (_: R) => true,
          filterOpt,
          None,
          Some(Ord),
          None
        )
  
      /**
        * Like Order By clause in SQL
        * ordering grouping criteria of type [[G]]
        * by some criteria [[B]]
        * having an implicit [[Ordering]] for [[B]]
        *
        * @param f - extract an ordering criteria
        **/
      def orderGroupsBy[B: Ordering](
          f: G => B
      ): MaybeOrderedHaving[Coll, A, G, T, R, Comb] = orderGroups(Ordering.by[G, B](f))
  
      /**
        * Like Order By clause in SQL
        * for aggregation result [[R]]
        *
        * @param Ord - implicit [[Ordering]] for [[R]]
        **/
      def orderAggregations(
          implicit Ord: Ordering[R]
      ): MaybeOrderedHaving[Coll, A, G, T, R, Comb] =
        new MaybeOrderedHaving(
          pipeline,
          getG,
          getT,
          (_: R) => true,
          filterOpt,
          None,
          None,
          Some(Ord)
        )
  
      /**
        * Like Order By clause in SQL
        * ordering aggregation result [[R]]
        * by some criteria [[B]]
        * having an implicit [[Ordering]] for [[B]]
        *
        * @param f - extract an ordering criteria
        **/
      def orderAggregationsBy[B: Ordering](
          f: R => B
      ): MaybeOrderedHaving[Coll, A, G, T, R, Comb] =
        orderAggregations(Ordering.by[R, B](f))
  
      /**
        * Order all QueryResuts
        * having [[Ordering]] for [[A]], [[G]] and [[R]]
        *
        * @param OrdA - implicit [[Ordering]] for [[A]]
        * @param OrdG - implicit [[Ordering]] for [[G]]
        * @param OrdR - implicit [[Ordering]] for [[R]]
        **/
      def ordered(implicit OrdA: Ordering[A], OrdG: Ordering[G], OrdR: Ordering[R]): MaybeOrderedHaving[Coll, A, G, T, R, Comb] =
        new MaybeOrderedHaving(
          pipeline,
          getG,
          getT,
          (_: R) => true,
          filterOpt,
          Some(OrdA),
          Some(OrdG),
          Some(OrdR)
        )

      def compile(
        given buildFrom1: BuildFrom[Coll[A], A, Coll[A]],
        Coll: Factory[QueryResult[Seq, A, G, R], Coll[QueryResult[Coll, A, G, R]]],
        Coll2: Factory[QueryResult[Coll, A, G, R], Coll[QueryResult[Coll, A, G, R]]]
      ) = Query[Coll, A, G, T, R, Comb](
          pipeline,
          getG,
          getT,
          (a: A) => filterOpt.forall(_.p(a)),
          (_: R) => true,
          None,
          None,
          None
      ).compile
    
  end Aggregate

  class MaybeOrderedHaving[Coll[x] <: Seq[x], A, G <: Tuple, T <: Tuple, R <: Tuple, Comb](
        override val pipeline: Coll[A],
        override val getG: A => G,
        override val getT: A => T,
        val havingF: R => Boolean,
        override val filterOpt: Option[Where[Coll, A]],
        val orderRecords: Option[Ordering[A]],
        val orderGroups: Option[Ordering[G]],
        val orderResults: Option[Ordering[R]]
    )(given aggF: AggFunc[T, R, Comb])
        extends Aggregate[Coll, A, G, T, R, Comb](pipeline, getG, getT, filterOpt) with

    override def compile(
      given buildFrom1: BuildFrom[Coll[A], A, Coll[A]],
      Coll: Factory[QueryResult[Seq, A, G, R], Coll[QueryResult[Coll, A, G, R]]],
      Coll2: Factory[QueryResult[Coll, A, G, R], Coll[QueryResult[Coll, A, G, R]]]
    ) = Query[Coll, A, G, T, R, Comb](
      pipeline,
      getG,
      getT,
      (a: A) => filterOpt.forall(_.p(a)),
      havingF,
      orderRecords,
      orderGroups,
      orderResults
    ).compile

  end MaybeOrderedHaving
end QueryBuilder
    