package traveler

import traveler.pdts.InstantiablePDT
import traveler.pdts.PDT
import traveler.pdts.PDTNumeric
import scala.compiletime.codeOf

opaque type CLong <: PDT[CLong.Mapping] = PDT[CLong.Mapping]

object CLong:
  type Mapping[T <: Target] <: PDTNumeric.IntegralTypes = T match
    case Target.LinuxX64 | Target.MacX64 => Long
    case Target.WinX64                   => Int

  given InstantiablePDT[Mapping, CLong] = InstantiablePDT.derive

  given PDTNumeric[Mapping, CLong] = PDTNumeric.derive
