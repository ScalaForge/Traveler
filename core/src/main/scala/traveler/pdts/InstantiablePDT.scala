package traveler.pdts

import traveler.Target
import traveler.Target.SupportedTargets

import scala.compiletime.summonInline
import scala.annotation.nowarn
import scala.reflect.ClassTag

trait InstantiablePDT[Mapping[_ <: Target] <: Matchable, T]:
  def unspecific(u: SumMapping[Mapping, SupportedTargets])(using
      t: Target
  ): Option[T]

  // def unspecificCastTo(p: PDT[Mapping]): T

  def apply[U <: Target](using t: U)(v: Mapping[U]): T

  def unwrap[U <: Target](using t: U)(p: T): Mapping[U]

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
          ClassTag[Mapping[T]]
        ], Option[P]](
          [T <: Target] =>
            (t: T) ?=>
              tup =>
                given ClassTag[Mapping[T]] = tup._1
                a match
                  case b: Mapping[T] =>
                    Some(conv(PDT(b)))
                  case _ => None
        )

      def unspecific(u: SumMapping[Mapping, SupportedTargets])(using
          t: Target
      ): Option[P] = validValue(u)

      def unspecificCastTo(p: PDT[Mapping]): P = conv(p)

      def apply[U <: Target](using t: U)(v: Mapping[U]): P =
        PDT[Mapping, P, U](using t, conv)(v)

      def unwrap[U <: Target](using t: U)(p: P): Mapping[U] =
        PDT.unwrap(p)
