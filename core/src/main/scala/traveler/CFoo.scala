package traveler

import traveler.pdts.PDT

opaque type CFoo <: PDT = PDT

object CFoo:
  type Mapping[T <: Target] = T match
    case Target.LinuxX64 | Target.MacX64 => Short
    case Target.WinX64                   => Int