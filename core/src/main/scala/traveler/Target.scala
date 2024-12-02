package traveler

import scala.reflect.TypeTest
import traveler.pdts.{Output, Curry}
import scala.compiletime.{
  erasedValue,
  summonInline,
  summonAll,
  error,
  uninitialized
}
import scala.compiletime.ops.any.ToString
import traveler.pdts.TupledApplication
import scala.annotation.switch

sealed trait Target:
  val id: Int
  val realId: Int

  inline def reveal[H[_ <: Target] <: Tuple, Out](
      fn: [T <: Target] => T ?=> H[T] => Out
  ): Out =
    (this.realId: @switch) match
      case 1 =>
        given Target.LinuxX64 = Target.LinuxX64
        val input = summonAll[H[Target.LinuxX64]]
        fn[Target.LinuxX64](input)
      case 2 =>
        given Target.MacX64 = Target.MacX64
        val input = summonAll[H[Target.MacX64]]
        fn[Target.MacX64](input)

      case 3 =>
        given Target.WinX64 = Target.WinX64
        val input = summonAll[H[Target.WinX64]]
        fn[Target.WinX64](input)

//sealed trait LinuxX64 extends Target { val id = 1; val realId: Int = id}

object Target:
  sealed class LinuxX64 private[Target] extends Target {
    val id: Int = 1; val realId: Int = id
  }
  sealed class WinX64 private[Target] extends Target {
    val id: Int = 2; val realId: Int = id
  }
  sealed class MacX64 private[Target] extends Target {
    val id: Int = 3; val realId: Int = id
  }
  private[Target] val LinuxX64: LinuxX64 = new LinuxX64()
  private[Target] val WinX64: WinX64 = new WinX64()
  private[Target] val MacX64: MacX64 = new MacX64()
  given target: Target =
    new LinuxX64 {}

  type SupportedTargets = (LinuxX64, WinX64, MacX64)

  class Curried[T <: Target]:
    def apply[A](fn: T ?=> A)(using
        TypeTest[Target, T]
    ): Target ?=> Option[A] =
      val target = summon[Target]

      target match
        case _target: T =>
          given T = _target
          Some(fn)

        case _ => None

  def assume[T <: Target]: Curried[T] = new Curried[T]
