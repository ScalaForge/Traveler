package traveler 

import scala.reflect.TypeTest
import traveler.Target.LinuxX64
import traveler.Target.MacX64
import traveler.Target.WinX64
import traveler.Target.Assumption
import traveler.pdts.{Output, Curry}
import scala.compiletime.{erasedValue, summonInline, summonAll, error, uninitialized}
import scala.compiletime.ops.any.ToString
import traveler.pdts.TupledApplication
import scala.annotation.switch

sealed trait Target:
  val id: Int
  val realId: Int

  inline def reveal[H[_ <: Target] <: Tuple, Out](fn: [T <: Target] => T ?=> H[T] => Out): Out = 
    (this.realId: @switch) match
      case 1 => 
        given LinuxX64.type = LinuxX64
        val input = summonAll[H[LinuxX64.type]]
        fn[LinuxX64.type](input)
      case 2 =>
        given MacX64.type = MacX64
        val input = summonAll[H[MacX64.type]]
        fn[MacX64.type](input)

      case 3 =>
        given WinX64.type = WinX64
        val input = summonAll[H[WinX64.type]]
        fn[WinX64.type](input)
      
    


object Target:
  case object LinuxX64 extends Target { val id = 1; val realId: Int = id }

  case object WinX64 extends Target { val id = 2; val realId: Int = id }

  case object MacX64 extends Target { val id = 3; val realId: Int = id }

  sealed case class Assumption[T <: Target] private[Target] (real: Target) extends Target:
    val id = 4
    val realId: Int = real.id

  type SupportedTargets = (LinuxX64.type, WinX64.type, MacX64.type)

  class Curried[T <: Target]:
    def apply[A](fn: Assumption[T] ?=> A)(using TypeTest[Target, T]): Target ?=> Option[A] = 
      val target = summon[Target]

      target match
        case _: T => 
          given Assumption[T] = new Assumption[T](target)// (Assumption[T](target): T)
          Some(fn)

        case _ => None

  def assume[T <: Target]: Curried[T] = new Curried[T]