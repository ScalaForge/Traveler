package traveler

import scala.compiletime.summonAll
import scala.annotation.switch

sealed trait Target:
  inline def reveal[H[_ <: Target] <: Tuple](using DummyImplicit)[Out](
      fn: [T <: Target] => T ?=> H[T] => Out
  ): Out =
    (this: @switch) match
      case given Target.LinuxX64 =>
        fn(summonAll[H[Target.LinuxX64]])
      case given Target.MacX64 =>
        fn(summonAll[H[Target.MacX64]])
      case given Target.WinX64 =>
        fn(summonAll[H[Target.WinX64]])

object Target{
  sealed class LinuxX64 private[Target] extends Target
  sealed class WinX64 private[Target] extends Target
  sealed class MacX64 private[Target] extends Target

  private[traveler] val LinuxX64: LinuxX64 = new LinuxX64()
  private[traveler] val WinX64: WinX64 = new WinX64()
  private[traveler] val MacX64: MacX64 = new MacX64()
  given platform: Target =
    LinuxX64

  type SupportedTargets = (LinuxX64, WinX64, MacX64)
}
