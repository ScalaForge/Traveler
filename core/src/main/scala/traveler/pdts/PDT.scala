package traveler.pdts

import traveler.Target
import scala.reflect.ClassTag
import traveler.pdts.PDTNumeric.IntegralTypes

opaque type PDT <: Matchable =
  Matchable

object PDT:
  def get[Mapping[_ <: Target] <: Matchable, P, T <: Target](using
      t: T,
      eqG: PDT =:= P
  )(v: Mapping[T]): P =
    eqG(v)

  def unwrap[Mapping[_ <: Target] <: Matchable, P, T <: Target](using
      t: T,
      ct: ClassTag[Mapping[T]],
      eqG: P =:= PDT
  )(v: P): Mapping[T] = eqG(v).asInstanceOf[Mapping[T]]

  def unwrapUnsafe[Mapping[_ <: Target] <: Matchable, P, T <: Target](using
      eqG: P =:= PDT
  )(v: P): Mapping[T] = v.asInstanceOf[Mapping[T]]

  def apply[P <: PDT](using
      DummyImplicit
  )[T <: Target](using
      t: T,
      mapping: PlatformDependentMapping[P, ?]
  )(v: mapping._M[T]) = mapping(v)
  def unspecific[P <: PDT](using
      t: Target,
      m: PlatformDependentMapping[P, ?]
  )(u: SumMapping[m._M, Target.SupportedTargets]): Option[P] = m.unspecific(u)
  def fromMinima[P <: PDT](using
      DummyImplicit
  )[M[_ <: Target] <: PDTNumeric.IntegralTypes](using
      target: Target,
      m: PlatformDependentIntegralMapping[P, M]
  )(value: MappingMinima[M, Target.SupportedTargets, Long]): P =
    m.fromMinima(value)

  extension [P <: PDT](a: P)(using mapping: PlatformDependentMapping[P, ?])
    def unwrap[T <: Target](using t: T) = mapping.unwrap(a)

  extension [P <: PDT](a: P)(using
      mapping: PlatformDependentIntegralMapping[P, ?]
  ) def toMaxima(using target: Target) = mapping.toMaxima(a)

  extension [P <: PDT](a: P)(using 
    numeric: PDTNumeric[P]
  ) 
    def +(b: P): P = numeric.add(a,b)
    def *(b: P): P = numeric.times(a,b)

