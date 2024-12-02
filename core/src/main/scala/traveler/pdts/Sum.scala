package traveler.pdts

import traveler.Target

type SumMapping[
    Mapping[_ <: Target] <: Matchable,
    TargetTup <: Tuple
] <: Matchable = TargetTup match
  case h *: t     => Mapping[h] | SumMapping[Mapping, t]
  case EmptyTuple => Nothing

//Double > Long > Float > Int > Short > Byte
//Double/Long > Float/Int > Short > Byte
//Double > Long > Long52 > Float > Int > Int23 > Short > Byte

//Double > Float > Long > Long52 > Int > Int23 > Short > Byte

type IntegralMinimum[
    A <: PDTNumeric.IntegralTypes,
    B <: PDTNumeric.IntegralTypes
] <: PDTNumeric.IntegralTypes = A match
  case Byte => Byte
  case Short =>
    B match
      case Byte => Byte
      case _    => Short
  case Int =>
    B match
      case Byte | Short => B
      case _            => Int
  case Long =>
    B match
      case Long => Long
      case _    => B

type MappingMinima[Mapping[
    _ <: Target
] <: NumericPDT.NumericTypes, TargetTup <: Tuple, Minima <: NumericPDT.NumericTypes] <: NumericPDT.NumericTypes =
  TargetTup match
    case h *: t =>
      MappingMinima[Mapping, t, IntegralMinimum[Mapping[h], Minima]]
    case EmptyTuple => Minima
