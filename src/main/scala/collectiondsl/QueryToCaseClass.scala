package collectiondsl

import scala.deriving._
import scala.compiletime.{erasedValue, error, code}
import scala.quoted._

trait QueryToCaseClass[Coll[x] <: Seq[x], A, G <: Tuple, R <: Tuple, U] with
  def (result: QueryResult[Coll, A, G, R]) convert: U


object QueryToCaseClass with

  type Dealias[L <: Tuple] <: Tuple = L match 
    case Unit => Unit
    case (a :@ u) *: t => a *: Dealias[t]
    case h *: t => h *: Dealias[t]
  

  inline def dealias[L <: Tuple](tuple: L): Dealias[L] = 
    def (x: Any) widen: Dealias[L] = x.asInstanceOf[Dealias[L]]
    inline erasedValue[L] match 
      case _: Unit => ().widen
      case _: ((a :@ _) *: t) => 
        val x = tuple.asInstanceOf[(a :@ _) *: t]
        val head *: tail = x
        (head.value *: dealias[t](tail)).widen

      case _: (h *: t) =>
        val x = tuple.asInstanceOf[h *: t]
        val head *: tail = x
        (head *: dealias[t](tail)).widen

  type ProductOfAux[A, B <: Tuple] = Mirror.ProductOf[A] { type MirroredElemTypes = B }
  
  type TupleOf[Coll[x] <: Seq[x], A, G <: Tuple, R <: Tuple] = Tuple.Concat[
    Tuple.Concat[Dealias[G], Dealias[R]],
    Coll[A] *: Unit
  ]
  
  inline given derived[Coll[x] <: Seq[x], A, G <: Tuple, R <: Tuple, U]: (given m: Mirror.Of[U]) => QueryToCaseClass[Coll, A, G, R, U] =
    type X  = TupleOf[Coll, A, G, R]
    inline m match
      case p: ProductOfAux[U, X] =>
        new QueryToCaseClass[Coll, A, G, R, U] with
          def (result: QueryResult[Coll, A, G, R]) convert: U =
            val k = dealias[G](result.keys)
            val t = dealias[R](result.totals)
            val shape = (k ++ t) ++ (result.values *: ())
            p.fromProduct(shape.asInstanceOf[Product])
            
      case _ => error(code"expected product, got $m")
  

implicit class converter[Coll[x] <: Seq[x], A, G <: Tuple, R <: Tuple](val self: Coll[QueryResult[Coll, A, G, R]]) extends AnyVal with 
  def as[U](given toCaseClass: QueryToCaseClass[Coll, A, G, R, U]): Seq[U] = 
    self.map(_.convert)