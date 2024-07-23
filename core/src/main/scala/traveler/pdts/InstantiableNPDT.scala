package traveler.pdts

import traveler.Target
import scala.compiletime.summonInline
import traveler.Target.SupportedTargets

trait InstantiableNPDT[Mapping[_ <: Target] <: NumericPDT.NumericTypes, T]
    extends InstantiablePDT[Mapping, T]

object InstantiableNPDT:
  inline def derive[Mapping[
      _ <: Target
  ] <: NumericPDT.NumericTypes, NP <: NumericPDT[Mapping]] =
    new InstantiableNPDT[Mapping, NP]:
      val conv = summonInline[NumericPDT[Mapping] =:= NP]
      def unspecific(u: SumMapping[Mapping, SupportedTargets])(using
          t: Target
      ): Option[NP] =
        NumericPDT.inlApply[Mapping, NP](using t, conv)(u)

      def apply[U <: Target](using t: U)(v: Mapping[RemoveAssumption[U]]): NP =
        NumericPDT[Mapping, NP, U](using t, conv)(v)
      def unwrap[U <: Target](using t: U)(p: NP): ValueType[U] =
        NumericPDT.unwrap[Mapping, NP, U](using t, conv.flip)(p).asInstanceOf[ValueType[U]]
