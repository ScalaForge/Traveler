package traveler.pdts

import traveler.pdts.PDTNumeric.NumericTypes
import traveler.Target
import traveler.Target.SupportedTargets
import traveler.Target.LinuxX64
import traveler.Target.WinX64
import traveler.Target.MacX64

import scala.compiletime.erasedValue
import traveler.pdts.NumericData.Extract
import scala.annotation.switch

opaque type NumericPDT[
    Mapping <: NumericMapping[?] & Singleton
] <: NumericTypes =
  Long

object NumericPDT:
  type NumericTypes = Byte | Short | Int | Long | Float | Double | Int23

  inline def unwrap[Mapping <: NumericMapping[?] & Singleton, NP <: NumericPDT[
    Mapping
  ], T <: Target](using
      t: T,
      eqG: NP =:= NumericPDT[Mapping],
      mapping: ValueOf[Mapping]
  )(v: NP): mapping.value._M[T] =
    val typ = mapping.value.fn(t)
    (typ: @switch) match
      case 1 => v.toByte.asInstanceOf[mapping.value._M[T]]
      case 2 => v.toShort.asInstanceOf[mapping.value._M[T]]
      case 3 => v.toInt.asInstanceOf[mapping.value._M[T]]
      case 4 => v.toLong.asInstanceOf[mapping.value._M[T]]
