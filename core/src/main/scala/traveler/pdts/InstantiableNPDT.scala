package traveler.pdts

import traveler.Target
import scala.compiletime.summonInline
import traveler.Target.SupportedTargets
import traveler.Target.LinuxX64
import traveler.Target.MacX64
import traveler.Target.WinX64
import scala.annotation.nowarn

trait InstantiableNPDT[Mapping[_ <: Target] <: PDTNumeric.IntegralTypes, T]
    extends InstantiablePDT[Mapping, T]:
  def fromMinima(value: MappingMinima[Mapping, SupportedTargets, Double])(using
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
          value: MappingMinima[Mapping, SupportedTargets, Double]
      )(using target: Target): P = ???
      def unspecific(u: SumMapping[Mapping, SupportedTargets])(using
          t: Target
      ): Option[P] = ipdt.unspecific(u)
      def unwrap[U <: Target](using t: U)(p: P): ValueType[U] = ipdt.unwrap(p)

  // inline def derive[Mapping[
  //     _ <: Target
  // ] <: NumericPDT.NumericTypes, NP <: NumericPDT[Mapping]] =
  //   new InstantiableNPDT[Mapping, NP]:
  //     val conv = summonInline[NumericPDT[Mapping] =:= NP]
  //     def unspecific(u: SumMapping[Mapping, SupportedTargets])(using
  //         t: Target
  //     ): Option[NP] =
  //       NumericPDT.inlApply[Mapping, NP](using t, conv)(u)

  //     def apply[U <: Target](using t: U)(v: Mapping[RemoveAssumption[U]]): NP =
  //       NumericPDT[Mapping, NP, U](using t, conv)(v)
  //     def unwrap[U <: Target](using t: U)(p: NP): ValueType[U] =
  //       NumericPDT.unwrap[Mapping, NP, U](using t, conv.flip)(p).asInstanceOf[ValueType[U]]
