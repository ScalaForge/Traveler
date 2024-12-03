package traveler

import traveler.pdts.PDT 
import traveler.pdts.PDTNumeric
import traveler.pdts.IntegralMapping

import compiletime.{codeOf, error}

opaque type CFoo <: PDT = PDT

object CFoo: 
  type M[T <: Target] = T match 
    case Target.LinuxX64 => Short 
    case Target.WinX64 => Int
    case Target.MacX64 => Long 

  given IntegralMapping[CFoo, M] = IntegralMapping.derive
  given PDTNumeric[CFoo] = PDTNumeric.derive

