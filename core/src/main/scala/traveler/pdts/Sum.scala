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

type IntegralMaximum[
  A <: PDTNumeric.IntegralTypes,
  B <: PDTNumeric.IntegralTypes
] <: PDTNumeric.IntegralTypes = A match 
  case Long => Long 
  case Int => 
    B match 
      case Long => Long 
      case _ => Int 
  case Short => 
    B match 
      case Long | Int => B
      case _ => Short
  case Byte => 
    B match 
      case Byte => Byte
      case _ => B
    

type MappingMinima[Mapping[
    _ <: Target
] <: PDTNumeric.IntegralTypes, TargetTup <: Tuple, Minima <: PDTNumeric.IntegralTypes] <: PDTNumeric.IntegralTypes =
  TargetTup match
    case h *: t =>
      MappingMinima[Mapping, t, IntegralMinimum[Mapping[h], Minima]]
    case EmptyTuple => Minima

type MappingMaxima[Mapping[_ <: Target] <: PDTNumeric.IntegralTypes, TargetTup <: Tuple, Maxima <: PDTNumeric.IntegralTypes] <: PDTNumeric.IntegralTypes =
  TargetTup match 
    case h *: t => 
      MappingMaxima[Mapping, t, IntegralMaximum[Mapping[h], Maxima]]
    case EmptyTuple => Maxima

trait ExtractMapping[P]:
  type Mapping[_ <: Target] <: Matchable

object ExtractMapping:
  def generate[_Mapping[_ <: Target] <: Matchable, P <: PDT]
      : ExtractMapping[P] = new ExtractMapping[P]:
    type Mapping[T <: Target] = _Mapping[T]
  given forAllPdts[_Mapping[_ <: Target] <: Matchable, P <: PDT]
      : (ExtractMapping[P] { type Mapping[T <: Target] = _Mapping[T] }) =
    new ExtractMapping[P] {
      type Mapping[T <: Target] = _Mapping[T]
    }

trait ExtractIntegralMapping[P] extends ExtractMapping[P]:
  type Mapping[_ <: Target] <: PDTNumeric.IntegralTypes

object ExtractIntegralMapping:
  given forSomePdts[_Mapping[_ <: Target] <: PDTNumeric.IntegralTypes, P <: PDT]
      : (ExtractIntegralMapping[P] {
        type Mapping[T <: Target] = _Mapping[T]
      }) =
    new ExtractIntegralMapping[P]:
      type Mapping[T <: Target] = _Mapping[T]
