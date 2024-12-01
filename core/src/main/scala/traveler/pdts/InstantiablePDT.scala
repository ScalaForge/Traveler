package traveler.pdts

import traveler.Target
import traveler.Target.SupportedTargets

import scala.compiletime.erasedValue
import scala.compiletime.summonInline
import traveler.Target.LinuxX64
import traveler.Target.WinX64
import scala.annotation.nowarn
import scala.reflect.ClassTag
import scala.annotation.switch

trait InstantiablePDT[Mapping[_ <: Target] <: Matchable, T]:
  type ValueType[T <: Target] = Mapping[RemoveAssumption[T]]
  def unspecific(u: SumMapping[Mapping, SupportedTargets])(using
      t: Target
  ): Option[T]

  // def unspecificCastTo(p: PDT[Mapping]): T

  def apply[U <: Target](using t: U)(v: ValueType[U]): T

  def unwrap[U <: Target](using t: U)(p: T): ValueType[U]

object InstantiablePDT:
  @nowarn(
    "msg=New anonymous class definition will be duplicated at each inline site"
  )
  inline def derive[Mapping[_ <: Target] <: Matchable, P <: PDT[Mapping]] =
    new InstantiablePDT[Mapping, P]:
      val conv = summonInline[PDT[Mapping] =:= P]
      def validValue(using
          t: Target
      )(a: SumMapping[Mapping, SupportedTargets]): Option[P] =
        t.reveal[[T <: Target] =>> Tuple1[
          ClassTag[Mapping[RemoveAssumption[T]]]
        ], Option[P]](
          [T <: Target] =>
            (t: T) ?=>
              tup =>
                given ClassTag[Mapping[RemoveAssumption[T]]] = tup._1
                a match
                  case b: Mapping[RemoveAssumption[T]] =>
                    Some(conv(PDT(b)))
                  case _ => None
        )

      def unspecific(u: SumMapping[Mapping, SupportedTargets])(using
          t: Target
      ): Option[P] = validValue(u)

      def unspecificCastTo(p: PDT[Mapping]): P = conv(p)

      def apply[U <: Target](using t: U)(v: Mapping[RemoveAssumption[U]]): P =
        PDT[Mapping, P, U](using t, conv)(v)

      def unwrap[U <: Target](using t: U)(p: P): Mapping[RemoveAssumption[U]] =
        PDT.unwrap(p)
