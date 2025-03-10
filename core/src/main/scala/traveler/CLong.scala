package traveler

import traveler.pdts.PDT
import traveler.pdts.PDTNumeric
import traveler.pdts.IntegralMapping

opaque type CLong <: PDT = PDT

object CLong:
  type M[T <: Target] = T match
    case Target.LinuxX64 | Target.MacX64 => Long
    case Target.WinX64                   => Int

  given inst: IntegralMapping[CLong, M] = IntegralMapping.derive

  given PDTNumeric[CLong] = PDTNumeric.derive

  def apply[T <: Target](using t: T)(v: M[T]) = PDT.get(v)
