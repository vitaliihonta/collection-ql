package collectiondsl

import scala.language.higherKinds
import collectiondsl.QueryBuilder.{Aggregate, GroupBy}


trait agg22[Coll[x] <: Seq[x], A, G <: Tuple] with
  self: GroupBy[Coll, A, G] =>

  /**
    * Like Group By clause in SQL
    *
    **/
  def aggregate[T, H <: TaggedAgg[_, _, _], R <: Tuple, Comb](
      f: A => H
  )(given aggF: AggFunc[H *: Unit, R, Comb]): Aggregate[Coll, A, G, H *: Unit, R, Comb] =
    new Aggregate[Coll, A, G, H *: Unit, R, Comb](pipeline, self.getG, a => f(a) *: (), self.filterOpt)

  def aggregate[T, H1 <: TaggedAgg[_, _, _], H2 <: TaggedAgg[_, _, _], R <: Tuple, Comb](
      f1: A => H1,
      f2: A => H2
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        Unit,
      R,
      Comb
    ]): Aggregate[Coll, A, G, H1 *: H2 *: Unit, R, Comb] =
    new Aggregate[Coll, A, G, H1 *: H2 *: Unit, R, Comb](pipeline, self.getG, a => f1(a) *: f2(a) *: (), self.filterOpt)

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        Unit,
      R,
      Comb
    ](pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
        (),
      self.filterOpt)

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        Unit,
      R,
      Comb
    ](pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
        (),
      self.filterOpt)

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        Unit,
      R,
      Comb
    ](pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
        (),
      self.filterOpt)

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        Unit,
      R,
      Comb
    ](pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
        (),
      self.filterOpt)

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        Unit,
      R,
      Comb
    ](pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
        (),
      self.filterOpt)

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      H8 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7,
      f8: A => H8
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      H8 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        Unit,
      R,
      Comb
    ](
      pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
          f8(a) *:
        (),
      self.filterOpt
    )

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      H8 <: TaggedAgg[_, _, _],
      H9 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7,
      f8: A => H8,
      f9: A => H9
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      H8 *:
      H9 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        Unit,
      R,
      Comb
    ](
      pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
          f8(a) *:
          f9(a) *:
        (),
      self.filterOpt
    )

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      H8 <: TaggedAgg[_, _, _],
      H9 <: TaggedAgg[_, _, _],
      H10 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7,
      f8: A => H8,
      f9: A => H9,
      f10: A => H10
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      H8 *:
      H9 *:
      H10 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        Unit,
      R,
      Comb
    ](
      pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
          f8(a) *:
          f9(a) *:
          f10(a) *:
        (),
      self.filterOpt
    )

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      H8 <: TaggedAgg[_, _, _],
      H9 <: TaggedAgg[_, _, _],
      H10 <: TaggedAgg[_, _, _],
      H11 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7,
      f8: A => H8,
      f9: A => H9,
      f10: A => H10,
      f11: A => H11
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      H8 *:
      H9 *:
      H10 *:
      H11 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        Unit,
      R,
      Comb
    ](
      pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
          f8(a) *:
          f9(a) *:
          f10(a) *:
          f11(a) *:
        (),
      self.filterOpt
    )

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      H8 <: TaggedAgg[_, _, _],
      H9 <: TaggedAgg[_, _, _],
      H10 <: TaggedAgg[_, _, _],
      H11 <: TaggedAgg[_, _, _],
      H12 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7,
      f8: A => H8,
      f9: A => H9,
      f10: A => H10,
      f11: A => H11,
      f12: A => H12
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      H8 *:
      H9 *:
      H10 *:
      H11 *:
      H12 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        Unit,
      R,
      Comb
    ](
      pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
          f8(a) *:
          f9(a) *:
          f10(a) *:
          f11(a) *:
          f12(a) *:
        (),
      self.filterOpt
    )

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      H8 <: TaggedAgg[_, _, _],
      H9 <: TaggedAgg[_, _, _],
      H10 <: TaggedAgg[_, _, _],
      H11 <: TaggedAgg[_, _, _],
      H12 <: TaggedAgg[_, _, _],
      H13 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7,
      f8: A => H8,
      f9: A => H9,
      f10: A => H10,
      f11: A => H11,
      f12: A => H12,
      f13: A => H13
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      H8 *:
      H9 *:
      H10 *:
      H11 *:
      H12 *:
      H13 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        Unit,
      R,
      Comb
    ](
      pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
          f8(a) *:
          f9(a) *:
          f10(a) *:
          f11(a) *:
          f12(a) *:
          f13(a) *:
        (),
      self.filterOpt
    )

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      H8 <: TaggedAgg[_, _, _],
      H9 <: TaggedAgg[_, _, _],
      H10 <: TaggedAgg[_, _, _],
      H11 <: TaggedAgg[_, _, _],
      H12 <: TaggedAgg[_, _, _],
      H13 <: TaggedAgg[_, _, _],
      H14 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7,
      f8: A => H8,
      f9: A => H9,
      f10: A => H10,
      f11: A => H11,
      f12: A => H12,
      f13: A => H13,
      f14: A => H14
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      H8 *:
      H9 *:
      H10 *:
      H11 *:
      H12 *:
      H13 *:
      H14 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        Unit,
      R,
      Comb
    ](
      pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
          f8(a) *:
          f9(a) *:
          f10(a) *:
          f11(a) *:
          f12(a) *:
          f13(a) *:
          f14(a) *:
        (),
      self.filterOpt
    )

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      H8 <: TaggedAgg[_, _, _],
      H9 <: TaggedAgg[_, _, _],
      H10 <: TaggedAgg[_, _, _],
      H11 <: TaggedAgg[_, _, _],
      H12 <: TaggedAgg[_, _, _],
      H13 <: TaggedAgg[_, _, _],
      H14 <: TaggedAgg[_, _, _],
      H15 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7,
      f8: A => H8,
      f9: A => H9,
      f10: A => H10,
      f11: A => H11,
      f12: A => H12,
      f13: A => H13,
      f14: A => H14,
      f15: A => H15
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      H8 *:
      H9 *:
      H10 *:
      H11 *:
      H12 *:
      H13 *:
      H14 *:
      H15 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        Unit,
      R,
      Comb
    ](
      pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
          f8(a) *:
          f9(a) *:
          f10(a) *:
          f11(a) *:
          f12(a) *:
          f13(a) *:
          f14(a) *:
          f15(a) *:
        (),
      self.filterOpt
    )

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      H8 <: TaggedAgg[_, _, _],
      H9 <: TaggedAgg[_, _, _],
      H10 <: TaggedAgg[_, _, _],
      H11 <: TaggedAgg[_, _, _],
      H12 <: TaggedAgg[_, _, _],
      H13 <: TaggedAgg[_, _, _],
      H14 <: TaggedAgg[_, _, _],
      H15 <: TaggedAgg[_, _, _],
      H16 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7,
      f8: A => H8,
      f9: A => H9,
      f10: A => H10,
      f11: A => H11,
      f12: A => H12,
      f13: A => H13,
      f14: A => H14,
      f15: A => H15,
      f16: A => H16
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        H16 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      H8 *:
      H9 *:
      H10 *:
      H11 *:
      H12 *:
      H13 *:
      H14 *:
      H15 *:
      H16 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        H16 *:
        Unit,
      R,
      Comb
    ](
      pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
          f8(a) *:
          f9(a) *:
          f10(a) *:
          f11(a) *:
          f12(a) *:
          f13(a) *:
          f14(a) *:
          f15(a) *:
          f16(a) *:
        (),
      self.filterOpt
    )

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      H8 <: TaggedAgg[_, _, _],
      H9 <: TaggedAgg[_, _, _],
      H10 <: TaggedAgg[_, _, _],
      H11 <: TaggedAgg[_, _, _],
      H12 <: TaggedAgg[_, _, _],
      H13 <: TaggedAgg[_, _, _],
      H14 <: TaggedAgg[_, _, _],
      H15 <: TaggedAgg[_, _, _],
      H16 <: TaggedAgg[_, _, _],
      H17 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7,
      f8: A => H8,
      f9: A => H9,
      f10: A => H10,
      f11: A => H11,
      f12: A => H12,
      f13: A => H13,
      f14: A => H14,
      f15: A => H15,
      f16: A => H16,
      f17: A => H17
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        H16 *:
        H17 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      H8 *:
      H9 *:
      H10 *:
      H11 *:
      H12 *:
      H13 *:
      H14 *:
      H15 *:
      H16 *:
      H17 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        H16 *:
        H17 *:
        Unit,
      R,
      Comb
    ](
      pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
          f8(a) *:
          f9(a) *:
          f10(a) *:
          f11(a) *:
          f12(a) *:
          f13(a) *:
          f14(a) *:
          f15(a) *:
          f16(a) *:
          f17(a) *:
        (),
      self.filterOpt
    )

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      H8 <: TaggedAgg[_, _, _],
      H9 <: TaggedAgg[_, _, _],
      H10 <: TaggedAgg[_, _, _],
      H11 <: TaggedAgg[_, _, _],
      H12 <: TaggedAgg[_, _, _],
      H13 <: TaggedAgg[_, _, _],
      H14 <: TaggedAgg[_, _, _],
      H15 <: TaggedAgg[_, _, _],
      H16 <: TaggedAgg[_, _, _],
      H17 <: TaggedAgg[_, _, _],
      H18 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7,
      f8: A => H8,
      f9: A => H9,
      f10: A => H10,
      f11: A => H11,
      f12: A => H12,
      f13: A => H13,
      f14: A => H14,
      f15: A => H15,
      f16: A => H16,
      f17: A => H17,
      f18: A => H18
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        H16 *:
        H17 *:
        H18 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      H8 *:
      H9 *:
      H10 *:
      H11 *:
      H12 *:
      H13 *:
      H14 *:
      H15 *:
      H16 *:
      H17 *:
      H18 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        H16 *:
        H17 *:
        H18 *:
        Unit,
      R,
      Comb
    ](
      pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
          f8(a) *:
          f9(a) *:
          f10(a) *:
          f11(a) *:
          f12(a) *:
          f13(a) *:
          f14(a) *:
          f15(a) *:
          f16(a) *:
          f17(a) *:
          f18(a) *:
        (),
      self.filterOpt
    )

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      H8 <: TaggedAgg[_, _, _],
      H9 <: TaggedAgg[_, _, _],
      H10 <: TaggedAgg[_, _, _],
      H11 <: TaggedAgg[_, _, _],
      H12 <: TaggedAgg[_, _, _],
      H13 <: TaggedAgg[_, _, _],
      H14 <: TaggedAgg[_, _, _],
      H15 <: TaggedAgg[_, _, _],
      H16 <: TaggedAgg[_, _, _],
      H17 <: TaggedAgg[_, _, _],
      H18 <: TaggedAgg[_, _, _],
      H19 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7,
      f8: A => H8,
      f9: A => H9,
      f10: A => H10,
      f11: A => H11,
      f12: A => H12,
      f13: A => H13,
      f14: A => H14,
      f15: A => H15,
      f16: A => H16,
      f17: A => H17,
      f18: A => H18,
      f19: A => H19
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        H16 *:
        H17 *:
        H18 *:
        H19 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      H8 *:
      H9 *:
      H10 *:
      H11 *:
      H12 *:
      H13 *:
      H14 *:
      H15 *:
      H16 *:
      H17 *:
      H18 *:
      H19 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        H16 *:
        H17 *:
        H18 *:
        H19 *:
        Unit,
      R,
      Comb
    ](
      pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
          f8(a) *:
          f9(a) *:
          f10(a) *:
          f11(a) *:
          f12(a) *:
          f13(a) *:
          f14(a) *:
          f15(a) *:
          f16(a) *:
          f17(a) *:
          f18(a) *:
          f19(a) *:
        (),
      self.filterOpt
    )

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      H8 <: TaggedAgg[_, _, _],
      H9 <: TaggedAgg[_, _, _],
      H10 <: TaggedAgg[_, _, _],
      H11 <: TaggedAgg[_, _, _],
      H12 <: TaggedAgg[_, _, _],
      H13 <: TaggedAgg[_, _, _],
      H14 <: TaggedAgg[_, _, _],
      H15 <: TaggedAgg[_, _, _],
      H16 <: TaggedAgg[_, _, _],
      H17 <: TaggedAgg[_, _, _],
      H18 <: TaggedAgg[_, _, _],
      H19 <: TaggedAgg[_, _, _],
      H20 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7,
      f8: A => H8,
      f9: A => H9,
      f10: A => H10,
      f11: A => H11,
      f12: A => H12,
      f13: A => H13,
      f14: A => H14,
      f15: A => H15,
      f16: A => H16,
      f17: A => H17,
      f18: A => H18,
      f19: A => H19,
      f20: A => H20
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        H16 *:
        H17 *:
        H18 *:
        H19 *:
        H20 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      H8 *:
      H9 *:
      H10 *:
      H11 *:
      H12 *:
      H13 *:
      H14 *:
      H15 *:
      H16 *:
      H17 *:
      H18 *:
      H19 *:
      H20 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        H16 *:
        H17 *:
        H18 *:
        H19 *:
        H20 *:
        Unit,
      R,
      Comb
    ](
      pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
          f8(a) *:
          f9(a) *:
          f10(a) *:
          f11(a) *:
          f12(a) *:
          f13(a) *:
          f14(a) *:
          f15(a) *:
          f16(a) *:
          f17(a) *:
          f18(a) *:
          f19(a) *:
          f20(a) *:
        (),
      self.filterOpt
    )

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      H8 <: TaggedAgg[_, _, _],
      H9 <: TaggedAgg[_, _, _],
      H10 <: TaggedAgg[_, _, _],
      H11 <: TaggedAgg[_, _, _],
      H12 <: TaggedAgg[_, _, _],
      H13 <: TaggedAgg[_, _, _],
      H14 <: TaggedAgg[_, _, _],
      H15 <: TaggedAgg[_, _, _],
      H16 <: TaggedAgg[_, _, _],
      H17 <: TaggedAgg[_, _, _],
      H18 <: TaggedAgg[_, _, _],
      H19 <: TaggedAgg[_, _, _],
      H20 <: TaggedAgg[_, _, _],
      H21 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7,
      f8: A => H8,
      f9: A => H9,
      f10: A => H10,
      f11: A => H11,
      f12: A => H12,
      f13: A => H13,
      f14: A => H14,
      f15: A => H15,
      f16: A => H16,
      f17: A => H17,
      f18: A => H18,
      f19: A => H19,
      f20: A => H20,
      f21: A => H21
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        H16 *:
        H17 *:
        H18 *:
        H19 *:
        H20 *:
        H21 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      H8 *:
      H9 *:
      H10 *:
      H11 *:
      H12 *:
      H13 *:
      H14 *:
      H15 *:
      H16 *:
      H17 *:
      H18 *:
      H19 *:
      H20 *:
      H21 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        H16 *:
        H17 *:
        H18 *:
        H19 *:
        H20 *:
        H21 *:
        Unit,
      R,
      Comb
    ](
      pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
          f8(a) *:
          f9(a) *:
          f10(a) *:
          f11(a) *:
          f12(a) *:
          f13(a) *:
          f14(a) *:
          f15(a) *:
          f16(a) *:
          f17(a) *:
          f18(a) *:
          f19(a) *:
          f20(a) *:
          f21(a) *:
        (),
      self.filterOpt
    )

  def aggregate[
      T,
      H1 <: TaggedAgg[_, _, _],
      H2 <: TaggedAgg[_, _, _],
      H3 <: TaggedAgg[_, _, _],
      H4 <: TaggedAgg[_, _, _],
      H5 <: TaggedAgg[_, _, _],
      H6 <: TaggedAgg[_, _, _],
      H7 <: TaggedAgg[_, _, _],
      H8 <: TaggedAgg[_, _, _],
      H9 <: TaggedAgg[_, _, _],
      H10 <: TaggedAgg[_, _, _],
      H11 <: TaggedAgg[_, _, _],
      H12 <: TaggedAgg[_, _, _],
      H13 <: TaggedAgg[_, _, _],
      H14 <: TaggedAgg[_, _, _],
      H15 <: TaggedAgg[_, _, _],
      H16 <: TaggedAgg[_, _, _],
      H17 <: TaggedAgg[_, _, _],
      H18 <: TaggedAgg[_, _, _],
      H19 <: TaggedAgg[_, _, _],
      H20 <: TaggedAgg[_, _, _],
      H21 <: TaggedAgg[_, _, _],
      H22 <: TaggedAgg[_, _, _],
      R <: Tuple,
      Comb
  ](
      f1: A => H1,
      f2: A => H2,
      f3: A => H3,
      f4: A => H4,
      f5: A => H5,
      f6: A => H6,
      f7: A => H7,
      f8: A => H8,
      f9: A => H9,
      f10: A => H10,
      f11: A => H11,
      f12: A => H12,
      f13: A => H13,
      f14: A => H14,
      f15: A => H15,
      f16: A => H16,
      f17: A => H17,
      f18: A => H18,
      f19: A => H19,
      f20: A => H20,
      f21: A => H21,
      f22: A => H22
  )(given aggF: AggFunc[
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        H16 *:
        H17 *:
        H18 *:
        H19 *:
        H20 *:
        H21 *:
        H22 *:
        Unit,
      R,
      Comb
    ]): Aggregate[
    Coll,
    A,
    G,
    H1 *:
      H2 *:
      H3 *:
      H4 *:
      H5 *:
      H6 *:
      H7 *:
      H8 *:
      H9 *:
      H10 *:
      H11 *:
      H12 *:
      H13 *:
      H14 *:
      H15 *:
      H16 *:
      H17 *:
      H18 *:
      H19 *:
      H20 *:
      H21 *:
      H22 *:
      Unit,
    R,
    Comb
  ] =
    new Aggregate[
      Coll,
      A,
      G,
      H1 *:
        H2 *:
        H3 *:
        H4 *:
        H5 *:
        H6 *:
        H7 *:
        H8 *:
        H9 *:
        H10 *:
        H11 *:
        H12 *:
        H13 *:
        H14 *:
        H15 *:
        H16 *:
        H17 *:
        H18 *:
        H19 *:
        H20 *:
        H21 *:
        H22 *:
        Unit,
      R,
      Comb
    ](
      pipeline,
      self.getG,
      a =>
        f1(a) *:
          f2(a) *:
          f3(a) *:
          f4(a) *:
          f5(a) *:
          f6(a) *:
          f7(a) *:
          f8(a) *:
          f9(a) *:
          f10(a) *:
          f11(a) *:
          f12(a) *:
          f13(a) *:
          f14(a) *:
          f15(a) *:
          f16(a) *:
          f17(a) *:
          f18(a) *:
          f19(a) *:
          f20(a) *:
          f21(a) *:
          f22(a) *:
        (),
      self.filterOpt
    )
