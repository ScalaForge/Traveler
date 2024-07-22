package traveler

import traveler.Target.{LinuxX64, WinX64, MacX64}
import traveler.pdts.InstantiablePDT
import traveler.pdts.PDT
import traveler.pdts.PDTNumeric
import scala.compiletime.codeOf

opaque type CLong <: PDT[CLong.Mapping] = PDT[CLong.Mapping]

object CLong:
  type Mapping[T <: Target] <: PDTNumeric.NumericTypes = T match 
    case LinuxX64.type | MacX64.type => Long
    case WinX64.type => Float

  given InstantiablePDT[Mapping, CLong] = InstantiablePDT.derive

  given PDTNumeric[Mapping, CLong] = PDTNumeric.derive

