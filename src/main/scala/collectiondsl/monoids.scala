package collectiondsl

import cats.Monoid
import cats.syntax.monoid._

given aggResMonoid[A, R, Comb]: (given aggFunc: AggFunc[A, R, Comb]) => Monoid[AggFunc.Result[R, Comb]] = 
  new Monoid[AggFunc.Result[R, Comb]] with 
    def empty = aggFunc.empty.extract
    def combine(x: AggFunc.Result[R, Comb], y: AggFunc.Result[R, Comb]): AggFunc.Result[R, Comb] = 
      x.combiner.combine(y.combiner).extract
