package traveler.pdts

import traveler.Target
import traveler.Target.SupportedTargets
import traveler.Target.WinX64
import traveler.Target.MacX64
import traveler.Target.LinuxX64
import scala.annotation.nowarn

trait PlatformDependentIntegralMapping[P, M[
    _ <: Target
] <: PDTNumeric.IntegralTypes]:
  def fromMinima(value: MappingMinima[M, SupportedTargets, Long])(using
      target: Target
  ): P

  def toMaxima(value: P)(using
      target: Target
  ): MappingMaxima[M, SupportedTargets, Byte]

  // def castTo[O, ](ex: Class[O])(using m: Mapping[O, ?]): O =

object PlatformDependentIntegralMapping:
  // @nowarn(
  //   "msg=New anonymous class definition will be duplicated at each inline site"
  // )
  inline def derive[P, M[_ <: Target] <: PDTNumeric.IntegralTypes](using
      conv: PlatformDependentMapping[P, M]
  ): PlatformDependentIntegralMapping[P, M] =
    new PlatformDependentIntegralMapping[P, M]:
      def withConvertOrCT[A](
          fn: [T <: Target] => T ?=> (
              NumConvert[M[T], MappingMinima[M, SupportedTargets, Long]],
              NumConvert[MappingMaxima[M, SupportedTargets, Byte], M[T]]
          ) => A
      )(using t: Target): A = t.reveal[
        [T <: Target] =>> (
            NumConvert[M[T], MappingMinima[M, SupportedTargets, Long]],
            NumConvert[MappingMaxima[M, SupportedTargets, Byte], M[T]]
        )
      ](
        [T <: Target] => (t: T) ?=> tup => fn(tup._1, tup._2)
      )

      def fromMinima(
          value: MappingMinima[M, SupportedTargets, Long]
      )(using target: Target): P = withConvertOrCT(
        [T <: Target] =>
          (t: T) ?=> (numConvMin, _) => conv(numConvMin(value))
      )

      def toMaxima(
          value: P
      )(using Target): MappingMaxima[M, SupportedTargets, Byte] =
        withConvertOrCT(
          [T <: Target] =>
            (t: T) ?=>
              (_, numConvMax) => numConvMax(conv.unwrap(using t)(value))
        )
