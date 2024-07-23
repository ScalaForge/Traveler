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
    Mapping <: NumericMapping & Singleton
] <: NumericTypes =
  Long

object NumericPDT:
  type NumericTypes = Byte | Short | Int | Long | Float | Double

  inline def unwrap[Mapping <: NumericMapping & Singleton, NP <: NumericPDT[
    Mapping
  ], T <: Target](using
      t: T,
      eqG: NP =:= NumericPDT[Mapping],
      mapping: ValueOf[Mapping]
  )(v: NP): mapping.value.M[T] = 
    val typ = mapping.value(t)
    (typ: @switch) match 
      case 1 => v.toByte.asInstanceOf[mapping.value.M[T]]
      case 2 => v.toShort.asInstanceOf[mapping.value.M[T]]
      case 3 => v.toInt.asInstanceOf[mapping.value.M[T]]
      case 4 => v.toLong.asInstanceOf[mapping.value.M[T]]

  inline def inlApply[Mapping <: NumericMapping & Singleton, NP <: NumericPDT[Mapping]](
    using t: Target,
    eqG: NumericPDT[Mapping] =:= NP,
    mapping: ValueOf[Mapping]
  )(v: SumMapping[mapping.value.M, SupportedTargets]): Option[NP] = 
    t match
      case LinuxX64 => 
        v match 
          case u: Mapping[LinuxX64.type] => Some(eqG(NumericData(u)))
          case _ => None
      case WinX64 => 
        v match
          case u: Mapping[WinX64.type] => Some(eqG(NumericData(u)))
          case _ => None 

      case MacX64 => 
        v match
          case u: Mapping[MacX64.type] => Some(eqG(NumericData(u)))
          case _ => None 

  def apply[Mapping[_ <: Target] <: NumericTypes, NP <: NumericPDT[Mapping], T <: Target](using 
    t: T,
    eqG: NumericPDT[Mapping] =:= NP
  )(v: Mapping[RemoveAssumption[T]]): NP = eqG(NumericData(v))

            
        
          
        
