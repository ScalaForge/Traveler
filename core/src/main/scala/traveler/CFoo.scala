package traveler

import traveler.pdts.PDT
import traveler.pdts.PDTNumeric
import traveler.pdts.IntegralMapping

opaque type CFoo <: PDT = PDT

object CFoo:
  type M[T <: Target] = T match
    case Target.LinuxX64 | Target.MacX64 => Short
    case Target.WinX64                   => Int

  given IntegralMapping[CFoo, M] = IntegralMapping.derive
  given PDTNumeric[CFoo] = PDTNumeric.derive
