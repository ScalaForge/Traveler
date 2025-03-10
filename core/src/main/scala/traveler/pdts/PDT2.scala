package traveler.pdts

import traveler.Target
import traveler.Target.LinuxX64
import traveler.Target.MacX64
import traveler.Target.WinX64
import scala.reflect.ClassTag

import language.experimental.erasedDefinitions
import scala.annotation.experimental



trait PDT2[Mapping[T <: Target] <: Matchable] extends Any:
  val value: Matchable

@experimental
object Test
  //sealed class CBar(override val value: Matchable) extends AnyVal, PDT2[Test.Mapping]// derives PlatformDependentMapping2
  //def unwrap[T <: Target](using T): Mapping[T] = value.asInstanceOf[Mapping[T]]

  // val p = new PlatformDependentMapping2[CBar]:
  //   def withCT[A](fn: [T <: Target] => T ?=> ClassTag[U[CBar][T]] => A)(using t: Target): A = t.reveal[
  //     [T <: Target] =>> Tuple1[ClassTag[U[CBar][T]]]
  //   ](
  //     [T <: Target] => t ?=> tup => fn(tup._1)
  //   )

  //   def unspecific(u: SumMapping[U[CBar], Target.SupportedTargets])(using t: Target): Option[CBar] = ???
  //   def unspecific3[M[_ <: Target] <: Matchable](using CBar <:< PDT2[M])(u: SumMapping[U[CBar], Target.SupportedTargets])(using t: Target): Option[CBar] = ???
  // val r = CBar(5)

  // type Mapping[T <: Target] = T match 
  //   case LinuxX64 => Int
  //   case MacX64 => Float 
  //   case WinX64 => Object
  // //val x = p.unspecific(5)