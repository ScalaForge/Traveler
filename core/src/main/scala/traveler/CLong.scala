package traveler

import traveler.pdts.PDT
import traveler.pdts.PlatformDependentMapping
import traveler.pdts.PDTNumeric

opaque type CLong <: PDT = PDT

object CLong:
  type Mapping[T <: Target] = T match
    case Target.LinuxX64 | Target.MacX64 => Long
    case Target.WinX64                   => Int

  given PlatformDependentMapping[CLong, Mapping] = PlatformDependentMapping.derive
  given PDTNumeric[CLong] = PDTNumeric.derive