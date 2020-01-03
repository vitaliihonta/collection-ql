package collectiondsl

import scala.compiletime.{erasedValue, error}
import scala.language.implicitConversions
import cats.Eq
import cats.implicits._

case class :@[A, U](value: A)
object :@ with
  given taggedEq[A, U]: (given Eq[A]) => Eq[A :@ U] =
    new Eq[A :@ U] with
      def eqv(x: :@[A, U], y: :@[A, U]): Boolean = 
        x.value === y.value


implicit class aliasing[A](val self: A) extends AnyVal with
  def as[U]: A :@ U = :@(self)
  def as(s: Singleton): A :@ s.type = as[s.type]

implicit class namedTupleOps[L <: Tuple](val self: L) extends AnyVal with
  inline def get[U] <: Any = inline erasedValue[L] match
    case _: Unit => error("not found")
    case _: ((a :@ U) *: t) =>
      val x = self.asInstanceOf[(a :@ U) *: t].head
      x.value

    case _: (h *: t) =>
      val x = self.asInstanceOf[h *: t].tail
      x.get[U]
