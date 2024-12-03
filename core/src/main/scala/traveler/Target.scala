package traveler

import scala.reflect.TypeTest
import scala.compiletime.{
  erasedValue,
  summonInline,
  summonAll,
  error,
  uninitialized
}
import scala.annotation.switch
import scala.Tuple.Fold

sealed trait Target:
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
    val realId: Int = 1
  }
  sealed class WinX64 private[Target] extends Target {
    val realId: Int = 2
  }
  sealed class MacX64 private[Target] extends Target {
    val realId: Int = 3
  }

  def assumption[T <: Target](
      assumptionSet: Set[Int],
      maybeT: Option[T]
  )(using t: Target): Option[T] =
    if assumptionSet.contains(t.realId) then
      maybeT.orElse(
        Some(
          new Target {
            val realId: Int = t.realId
          }.asInstanceOf[T]
        )
      )
    else None

  private[Target] val LinuxX64: LinuxX64 = new LinuxX64()
  private[Target] val WinX64: WinX64 = new WinX64()
  private[Target] val MacX64: MacX64 = new MacX64()
  given target: Target =
    new LinuxX64 {}

  private inline def assumptionSet[T <: Tuple]: Set[Int] =
    inline erasedValue[T] match
      case _: (LinuxX64 *: t) => assumptionSet[t] + LinuxX64.realId
      case _: (MacX64 *: t)   => assumptionSet[t] + MacX64.realId
      case _: (WinX64 *: t)   => assumptionSet[t] + WinX64.realId
      case _: EmptyTuple      => Set.empty

  type SupportedTargets = (LinuxX64, WinX64, MacX64)

  class Curried[T](assumptionSet: Set[Int], t: Option[T])(using
      ev: T <:< Target
  ):
    def apply[A](fn: T ?=> A)(using
        TypeTest[Target, T]
    ): Target ?=> Option[A] =
      assumption(assumptionSet, t.map(ev)).map(a => fn(using a.asInstanceOf[T]))

  inline def assume[T <: Tuple](using
      Tuple.Union[T] <:< Target
  ): Curried[Tuple.Union[T]] =
    val u: Option[Tuple.Union[T]] = getExemplar[Tuple.Union[T]]
    new Curried[Tuple.Union[T]](assumptionSet[T], u)

  private inline def getExemplar[T]: Option[T] =
    (inline erasedValue[T] match
      case _: LinuxX64 => Some(LinuxX64)
      case _: MacX64   => Some(MacX64)
      case _: WinX64   => Some(WinX64)
      case _           => None
    ).asInstanceOf[Option[T]]

  inline def assume[T <: Target](using
      Tuple.Contains[SupportedTargets, T] =:= true
  ): Curried[T] = new Curried[T](assumptionSet[Tuple1[T]], getExemplar[T])
  // def assume[T <: Target]: Curried[T] = new Curried[T]
  private[traveler] inline def test[T <: Target]: T =
    inline erasedValue[T] match
      case _: LinuxX64 => LinuxX64.asInstanceOf[T]
      case _: MacX64   => MacX64.asInstanceOf[T]
      case _: WinX64   => WinX64.asInstanceOf[T]
      case _           => error("Type not supported")
