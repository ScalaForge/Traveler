package traveler.pdts

import traveler.Target
import traveler.Target.SupportedTargets
import scala.reflect.ClassTag
import traveler.Target.WinX64
import traveler.Target.MacX64
import traveler.Target.LinuxX64
import scala.annotation.nowarn

trait IntegralMapping[P, M[_ <: Target] <: PDTNumeric.IntegralTypes]
    extends Mapping[P, M]:
  def fromMinima(value: MappingMinima[M, SupportedTargets, Long])(using
      target: Target
  ): P

  def toMaxima(value: P)(using
      target: Target
  ): MappingMaxima[M, SupportedTargets, Byte]

  // def castTo[O, ](ex: Class[O])(using m: Mapping[O, ?]): O =

object IntegralMapping:
  @nowarn(
    "msg=New anonymous class definition will be duplicated at each inline site"
  )
  inline def derive[P, M[_ <: Target] <: PDTNumeric.IntegralTypes](using
      conv: PDT =:= P
  ): IntegralMapping[P, M] =
    ((new IntegralMapping[P, M]:
      def withConvertOrCT[A](
          fn: [T <: Target] => T ?=> (
              NumConvert[M[T], MappingMinima[M, SupportedTargets, Long]],
              NumConvert[MappingMaxima[M, SupportedTargets, Byte], M[T]],
              ClassTag[M[T]]
          ) => A
      )(using t: Target): A = t.reveal[
        [T <: Target] =>> (
            NumConvert[M[T], MappingMinima[M, SupportedTargets, Long]],
            NumConvert[MappingMaxima[M, SupportedTargets, Byte], M[T]],
            ClassTag[M[T]]
        ),
        A
      ](
        [T <: Target] => (t: T) ?=> tup => fn(tup._1, tup._2, tup._3)
      )

      def fromMinima(
          value: MappingMinima[M, SupportedTargets, Long]
      )(using target: Target): P = withConvertOrCT(
        [T <: Target] =>
          (t: T) ?=> (numConvMin, _, _) => PDT.get(numConvMin(value))
      )

      def toMaxima(
          value: P
      )(using Target): MappingMaxima[M, SupportedTargets, Byte] =
        withConvertOrCT(
          [T <: Target] =>
            (t: T) ?=>
              (_, numConvMax, ct) => numConvMax(unwrap(using t, ct)(value))
        )

      def apply[T <: Target](using t: T)(v: M[T]): P = PDT.get[M, P, T](v)
      def unwrap[T <: Target](using t: T, ct: ClassTag[M[T]])(v: P): M[T] =
        PDT.unwrap[M, P, T](using t, ct, conv.flip)(v)
      def unspecific(
          u: SumMapping[M, SupportedTargets]
      )(using t: Target): Option[P] =
        withConvertOrCT(
          [T <: Target] =>
            (t: T) ?=>
              (_, _, ct) =>
                given ClassTag[M[T]] = ct
                u match
                  case b: M[T] => Some(PDT.get(b))
                  case _       => None
        )
    ))
