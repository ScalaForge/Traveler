package traveler.pdts

import traveler.Target
import traveler.Target.LinuxX64
import scala.compiletime.summonInline
import traveler.Target.MacX64
import traveler.Target.WinX64
import traveler.Target.Assumption
import scala.annotation.switch

trait PDTNumeric[
    Mapping[_ <: Target] <: PDTNumeric.IntegralTypes,
    P <: PDT[Mapping]
]:
  val inst: InstantiablePDT[Mapping, P]
  def add(a: P, b: P): Target ?=> P
  inline def addSpecific[T <: Target](using T)(using
      num: Numeric[inst.ValueType[T]]
  )(a: P, b: P): P =
    inst(
      num.plus(
        a.asInstanceOf[Mapping[RemoveAssumption[T]]],
        b.asInstanceOf[Mapping[RemoveAssumption[T]]]
      )
    )

  def times(a: P, b: P): Target ?=> P
  inline def timesSpecific[T <: Target](using T)(using
      num: Numeric[inst.ValueType[T]]
  )(a: P, b: P): P =
    num
      .plus(
        a.asInstanceOf[inst.ValueType[T]],
        b.asInstanceOf[inst.ValueType[T]]
      )
      .asInstanceOf[P]
    // inst(num.plus(inst.unwrap(a), inst.unwrap(b)))

object PDTNumeric:
  type NumericTypes = Float | Double | IntegralTypes
  type IntegralTypes = Byte | Short | Int | Long
  inline def derive[
      Mapping[_ <: Target] <: IntegralTypes,
      P <: PDT[Mapping]
  ](using it: InstantiablePDT[Mapping, P], eqG: P =:= PDT[Mapping]) =
    new PDTNumeric[Mapping, P]:
      val inst = it

      def withContext[A](
          fn: [T <: Target] => (
              t: T
          ) ?=> Numeric[Mapping[RemoveAssumption[T]]] => A
      )(using t: Target) =
        t.reveal[[T <: Target] =>> Tuple1[
          Numeric[Mapping[RemoveAssumption[T]]]
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
