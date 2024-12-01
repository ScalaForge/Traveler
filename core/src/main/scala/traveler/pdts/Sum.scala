package traveler.pdts

import traveler.Target
import traveler.Target.Assumption

type SumMapping[Mapping[_ <: Target] <: Matchable, TargetTup <: Tuple] <: Matchable = TargetTup match 
  case h *: t => Mapping[h] | SumMapping[Mapping, t]
  case EmptyTuple => Nothing

type RemoveAssumption[T <: Target] <: Target = T match 
  case Assumption[a] => a
  case _ => T


//Double > Long > Float > Int > Short > Byte
//Double/Long > Float/Int > Short > Byte
//Double > Long > Long52 > Float > Int > Int23 > Short > Byte

//Double > Float > Long > Long52 > Int > Int23 > Short > Byte

type NumericMinimum[A <: NumericPDT.NumericTypes, B <: NumericPDT.NumericTypes] <: NumericPDT.NumericTypes = A match 
  case Byte => Byte
  case Short => B match 
    case Byte => Byte 
    case _ => Short
  case Int => B match 
    case Byte | Short | Int23 => B 
    case Float | Double => Int23
    case _ => Int
  case Long => B match 
    case Int | Short | Byte | Int23 => B 
    case Float | Double => Int23 //Add LONG52
    case _ => Long 
  case Float => B match 
    case Short | Byte => B 
    case Int | Long => Int23
    case _ => Float
  case Double => B match 
    case Double => Double 
    //case Long => Long52
    case  _ => B

type MappingMinima[Mapping[_ <: Target] <: NumericPDT.NumericTypes, TargetTup <: Tuple, Minima <: NumericPDT.NumericTypes] <: NumericPDT.NumericTypes = TargetTup match
  case h *: t => MappingMinima[Mapping, t, NumericMinimum[Mapping[h], Minima]]
  case EmptyTuple => Minima


