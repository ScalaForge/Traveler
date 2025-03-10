package traveler.pdts

import traveler.Target
import scala.compiletime.summonInline
import annotation.nowarn
import traveler.Target.SupportedTargets
import traveler.SpecificTarget
import traveler.pdts.PDTNumeric.IntegralTypes

trait PDTNumeric[
    P
]:
  def add(a: P, b: P): Target ?=> P
  def times(a: P, b: P): Target ?=> P

object PDTNumeric:
  type NumericTypes = Float | Double | IntegralTypes
  type IntegralTypes = Byte | Short | Int | Long

  @nowarn(
    "msg=New anonymous class definition will be duplicated at each inline site"
  )
  inline def derive[
      P,
      M[_ <: Target] <: PDTNumeric.IntegralTypes
  ](using pdm: PlatformDependentMapping[P,M]) =
    new PDTNumeric[P]:
      def withContext[A](
          fn: [T <: Target] => (
              t: T
          ) ?=> Numeric[M[T]] => A
      )(using t: Target) =
        t.reveal[[T <: Target] =>> Tuple1[
          Numeric[M[T]]
        ]](
          [T <: Target] => (t: T) ?=> tup => fn[T](tup._1)
        )

      def add(a: P, b: P): Target ?=> P =
        withContext(
          [T <: Target] =>
            T ?=>
              num => pdm(num.plus(pdm.unwrap(a), pdm.unwrap(b)))
        )

      def times(a: P, b: P): Target ?=> P =
        withContext(
          [T <: Target] =>
            T ?=>
              num => pdm(num.times(pdm.unwrap(a), pdm.unwrap(b)))
        )

// abstract class DerivePDTNumeric[P <: PDT] extends DerivePDT[P]:
//   type Mapping[_ <: Target] <: PDTNumeric.IntegralTypes
//   given pdim: PlatformDependentIntegralMapping[P, Mapping] =
//     PlatformDependentIntegralMapping.derive
//   given pdtNumeric: PDTNumeric[P] = PDTNumeric.derive(using pdm)

//   def apply[T <: Target](using
//       t: T,
//       pdm: PlatformDependentMapping[P, Mapping]
//   )(v: Mapping[T]): P = pdm.apply(v)
//   def unspecific(u: SumMapping[Mapping, SupportedTargets])(using
//       t: Target,
//       pdm: PlatformDependentMapping[P, Mapping]
//   ): Option[P] = pdm.unspecific(u)
//   def fromMinima(value: MappingMinima[Mapping, SupportedTargets, Long])(using
//       target: Target,
//       pdim: PlatformDependentIntegralMapping[P, Mapping]
//   ): P = pdim.fromMinima(value)

//   extension (a: P)(using num: PDTNumeric[P])
//     def +(b: P): P = num.add(a, b)
//     def *(b: P): P = num.times(a, b)

//   extension (a: P)
//     def +&[T <: Target, M[_ <: Target] <: IntegralTypes](b: P)(using
//       t: T,
//       targetEvidence: SpecificTarget[T],
//       pdm: PlatformDependentMapping[P, M],
//       num: Numeric[M[T]]
//     ): P = pdm(num.plus(pdm.unwrap(a), pdm.unwrap(b)))
//     def *&[T <: Target, M[_ <: Target] <: IntegralTypes](b: P)(using
//       t: T,
//       targetEvidence: SpecificTarget[T],
//       pdm: PlatformDependentMapping[P, M],
//       num: Numeric[M[T]]
//     ): P = pdm(num.times(pdm.unwrap(a), pdm.unwrap(b)))
