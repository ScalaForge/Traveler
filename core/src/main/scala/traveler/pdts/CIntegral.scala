package traveler.pdts

import traveler.Target
import traveler.Target.SupportedTargets
import traveler.Target.LinuxX64
import traveler.Target.WinX64
import scala.reflect.TypeTest
import traveler.Target.MacX64
import scala.annotation.nowarn

opaque type PDT[TargetMapping[_ <: Target] <: Matchable] <: Matchable =
  Matchable

object PDT:
  @nowarn("msg=Unreachable case except for null")
  inline def inlApply[Mapping[_ <: Target] <: Matchable, P <: PDT[Mapping]](
      using
      t: Target,
      eqG: PDT[Mapping] =:= P
  )(v: SumMapping[Mapping, SupportedTargets]): Option[P] =
    t match
      case LinuxX64 =>
        v match
          case u: Mapping[LinuxX64.type] => Some(eqG(u))
          case _                    => None

      case WinX64 =>
        v match
          case u: Mapping[WinX64.type] => Some(eqG(u))
          case _                  => None

      case MacX64 =>
        v match
          case u: Mapping[MacX64.type] => Some(eqG(u))
          case _                  => None

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
    
  
