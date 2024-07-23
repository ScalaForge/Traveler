package traveler.pdts

import traveler.Target
import traveler.Target.Assumption

type SumMapping[Mapping[_ <: Target] <: Matchable, TargetTup <: Tuple] <: Matchable = TargetTup match 
  case h *: t => Mapping[h] | SumMapping[Mapping, t]
  case EmptyTuple => Nothing

type RemoveAssumption[T <: Target] <: Target = T match 
  case Assumption[a] => a
  case _ => T