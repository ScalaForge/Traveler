package traveler.pdts

import traveler.Target
import scala.reflect.ClassTag

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
  )(v: P): Mapping[T] = eqG(v) match
    case res: Mapping[T] => res
    case _               => throw new Error("I shouldn't be here")

  extension [P <: PDT](a: P)(using num: PDTNumeric[P])
    def +(b: P): P = num.add(a, b)
    def *(b: P): P = num.times(a, b)


  def apply[P<: PDT](using DummyImplicit)[T <: Target](using t: T, mapping: Mapping[P,?])(v: mapping._M[T]) = mapping(v)
  def unspecific[P <: PDT](using t: Target, m: Mapping[P, ?])(u: SumMapping[m._M, Target.SupportedTargets]): Option[P] = m.unspecific(u)
  def fromMinima[P <: PDT](using DummyImplicit)[M[_ <: Target] <: PDTNumeric.IntegralTypes](using target: Target, m: IntegralMapping[P,M])(value: MappingMinima[M, Target.SupportedTargets, Long]): P = m.fromMinima(value)

  extension [P <: PDT](a: P)(using mapping: Mapping[P, ?])
    def unwrap[T <: Target](using t: T) = mapping.unwrap(a)

  extension [P <: PDT](a: P)(using mapping: IntegralMapping[P, ?])
    def toMaxima(using target: Target) = mapping.toMaxima(a)
