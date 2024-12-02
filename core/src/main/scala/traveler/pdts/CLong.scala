package traveler

import traveler.pdts.PDT
import traveler.pdts.PDTNumeric
import traveler.pdts.IntegralMapping

import compiletime.{codeOf, error}

opaque type CLong <: PDT = PDT

opaque type MT >: Int = Int

object MT:
  def apply(i: Int): MT = i

object CLong:
  type M[T <: Target] = T match
    case Target.LinuxX64 | Target.MacX64 => Long
    case Target.WinX64                   => Int
  given inst: IntegralMapping[CLong, M] = IntegralMapping.derive
  // given inst: InstantiablePDT[CLong] = InstantiablePDT.derive(using mapping)

  given PDTNumeric[CLong] = PDTNumeric.derive
