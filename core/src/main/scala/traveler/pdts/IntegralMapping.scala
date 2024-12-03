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
              ClassTag[M[T]]
          ) => A
      )(using t: Target): A = t.reveal[
        [T <: Target] =>> (
            NumConvert[M[T], MappingMinima[M, SupportedTargets, Long]],
            ClassTag[M[T]]
        ),
        A
      ](
        [T <: Target] => (t: T) ?=> tup => fn(tup._1, tup._2)
      )

      def fromMinima(
          value: IntegralMinimum[M[MacX64], IntegralMinimum[M[
            WinX64
          ], IntegralMinimum[M[LinuxX64], Long]]]
      )(using target: Target): P = withConvertOrCT(
        [T <: Target] => (t: T) ?=> (numConv, _) => PDT.get(numConv(value))
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
              (_, ct) =>
                given ClassTag[M[T]] = ct
                u match
                  case b: M[T] => Some(PDT.get(b))
                  case _       => None
        )
    ))
