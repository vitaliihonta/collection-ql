package collectiondsl

import scala.language.implicitConversions

class aggDsl[A, R, AggT <: AggFunc.Type](val `f`: A => R) extends AnyVal with
  def as(t: Singleton): A => TaggedAgg[R, t.type, AggT] = a => TaggedAgg(:@(`f`(a)))

class tagDsl[A, R](val `f`: A => R) extends AnyVal with
  def as(t: Singleton): A => R :@ t.type = a => :@(`f`(a))
  def agg[AggT <: AggFunc.Type](aggT: AggT): aggDsl[A, R, AggT] = new aggDsl[A, R, AggT](`f`)

class exprDsl[A](val `dummy`: Boolean = false) extends AnyVal with
  def apply[R](f: A => R): tagDsl[A, R] = new tagDsl[A, R](f)

def expr[A]: exprDsl[A]  = new exprDsl[A]()
def col[A]: tagDsl[A, A] = new tagDsl[A, A](identity)

given startQl[Coll[x] <: Seq[x], A]: Conversion[Coll[A], QueryBuilder.Empty[Coll, A]] = 
  new QueryBuilder.Empty[Coll, A](_)