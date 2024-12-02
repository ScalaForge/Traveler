package traveler

import traveler.pdts.InstantiablePDT
import traveler.pdts.PDT
import traveler.pdts.PDTNumeric
import scala.compiletime.codeOf
import traveler.pdts.NumericPDT
import traveler.pdts.NumericData
import traveler.pdts.NumericPDT.NumericTypes
import traveler.pdts.RemoveAssumption
import traveler.pdts.InstantiableNPDT
import traveler.pdts.MappingMinima

opaque type CLong <: PDT[CLong.Mapping] = PDT[CLong.Mapping]

// opaque type CLongNumeric <: NumericPDT[CLong.Mapping] =
//   NumericPDT[CLong.Mapping]

object CLong:
  type Mapping[T <: Target] <: PDTNumeric.IntegralTypes = T match
    case Target.LinuxX64 | Target.MacX64 => Long
    case Target.WinX64                   => Int

  val x: MappingMinima[Mapping, Target.SupportedTargets, Long] = 5

  given InstantiablePDT[Mapping, CLong] = InstantiablePDT.derive

  given PDTNumeric[Mapping, CLong] = PDTNumeric.derive

  // def num[T <: Target](using t: T)(
  //     v: Mapping[RemoveAssumption[T]]
  // ): CLongNumeric = NumericPDT[CLong.Mapping, CLongNumeric, T](v)

  // given InstantiableNPDT[Mapping, CLongNumeric] = ???//InstantiableNPDT.derive
