package pl.shumikowo.s1pb

import cats.syntax.apply._
import cats.instances.option._
import cats.kernel.Eq
import magnolia._

import scala.language.experimental.macros

object AutoEq {

  type Typeclass[T] = Eq[T]

  def combine[T](ctx: CaseClass[Eq, T]): Eq[T] =
    Eq.instance((a, b) => ctx.parameters.forall(p => p.typeclass.eqv(p.dereference(a), p.dereference(b))))

  def dispatch[T](ctx: SealedTrait[Eq, T]): Eq[T] =
    Eq.instance((a, b) =>
      ctx.dispatch(a)(sub =>
        (sub.cast.lift.apply(a), sub.cast.lift.apply(b)).tupled
          .fold(false) { case (a2, b2) => sub.typeclass.eqv(a2, b2) }
      )
    )

  implicit def gen[T]: Eq[T] = macro Magnolia.gen[T]
}
