package traveler.pdts

import traveler.Target
import traveler.Target.SupportedTargets
import traveler.Target.LinuxX64
import traveler.Target.WinX64
import scala.reflect.TypeTest
import traveler.Target.MacX64
import scala.annotation.nowarn
import scala.annotation.switch

opaque type PDT[TargetMapping[_ <: Target] <: Matchable] <: Matchable =
  Matchable

object PDT:
  def apply[Mapping[_ <: Target] <: Matchable, P <: PDT[Mapping], T <: Target](
      using
      t: T,
      eqG: PDT[Mapping] =:= P
  )(v: Mapping[RemoveAssumption[T]]): P =
    eqG(v)

  def unwrap[Mapping[_ <: Target] <: Matchable, P <: PDT[Mapping], T <: Target](
      using
      t: T,
      eqG: P =:= PDT[Mapping]
  )(v: P): Mapping[RemoveAssumption[T]] = eqG(v) match
    case res: Mapping[RemoveAssumption[T]] => res
    case _ => throw new Error("I shouldn't be here")
