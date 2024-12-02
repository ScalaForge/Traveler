package traveler.pdts

import traveler.Target
import scala.compiletime.summonInline
import traveler.Target.SupportedTargets
import scala.annotation.nowarn

trait InstantiableNPDT[Mapping[_ <: Target] <: PDTNumeric.IntegralTypes, T]
    extends InstantiablePDT[Mapping, T]:
  def fromMinima(value: MappingMinima[Mapping, SupportedTargets, Long])(using
      target: Target
  ): T

object InstantiableNPDT:
  @nowarn(
    "msg=New anonymous class definition will be duplicated at each inline site"
  )
  inline def derive[Mapping[_ <: Target] <: PDTNumeric.IntegralTypes, P <: PDT[
    Mapping
  ]] =
    new InstantiableNPDT[Mapping, P]:
      val minimaNum =
        summonInline[Numeric[MappingMinima[Mapping, SupportedTargets, Long]]]
      val ipdt = InstantiablePDT.derive[Mapping, P]
      def apply[U <: Target](using t: U)(v: Mapping[U]): P =
        ipdt.apply(v)
      def fromMinima(
          value: MappingMinima[Mapping, SupportedTargets, Long]
      )(using target: Target): P = target.reveal[[T <: Target] =>> Tuple1[
        NumConvert[Mapping[T], MappingMinima[Mapping, SupportedTargets, Long]]
      ], P](
        [T <: Target] => (t: T) ?=> tup => ipdt(tup._1(value))
      )
      def unspecific(u: SumMapping[Mapping, SupportedTargets])(using
          t: Target
      ): Option[P] = ipdt.unspecific(u)
      def unwrap[U <: Target](using t: U)(p: P): Mapping[U] = ipdt.unwrap(p)
