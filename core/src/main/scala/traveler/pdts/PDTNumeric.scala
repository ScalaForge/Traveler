package traveler.pdts

import traveler.Target
import scala.compiletime.summonInline
import annotation.nowarn

trait PDTNumeric[
    Mapping[_ <: Target] <: PDTNumeric.IntegralTypes,
    P <: PDT[Mapping]
]:
  val inst: InstantiablePDT[Mapping, P]
  def add(a: P, b: P): Target ?=> P
  def times(a: P, b: P): Target ?=> P

object PDTNumeric:
  type NumericTypes = Float | Double | IntegralTypes
  type IntegralTypes = Byte | Short | Int | Long
  @nowarn(
    "msg=New anonymous class definition will be duplicated at each inline site"
  )
  inline def derive[
      Mapping[_ <: Target] <: IntegralTypes,
      P <: PDT[Mapping]
  ](using it: InstantiablePDT[Mapping, P], eqG: P =:= PDT[Mapping]) =
    new PDTNumeric[Mapping, P]:
      val inst = it

      def withContext[A](
          fn: [T <: Target] => (
              t: T
          ) ?=> Numeric[Mapping[T]] => A
      )(using t: Target) =
        t.reveal[[T <: Target] =>> Tuple1[
          Numeric[Mapping[T]]
        ], A](
          [T <: Target] => (t: T) ?=> tup => fn[T](tup._1)
        )

      def add(a: P, b: P): Target ?=> P =
        withContext(
          [T <: Target] =>
            T ?=> num => inst(num.plus(inst.unwrap(a), inst.unwrap(b)))
        )

      def times(a: P, b: P): Target ?=> P =
        withContext(
          [T <: Target] =>
            T ?=> num => inst(num.times(inst.unwrap(a), inst.unwrap(b)))
        )
