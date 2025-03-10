package traveler.pdts

import traveler.CompanionClass
import traveler.Target.SupportedTargets
import traveler.Target
import scala.quoted.*
import traveler.Target.LinuxX64
import traveler.Target.MacX64
import traveler.Target.WinX64
import scala.reflect.ClassTag
import language.experimental.erasedDefinitions
import scala.annotation.experimental

trait PlatformDependentMapping2[P]:
  @experimental
  def unspecific(
      u: SumMapping[U[P], SupportedTargets]
  )(using t: Target): Option[P]

  def unspecific3[M <: [_ <: Target] =>> Matchable](using P <:< PDT2[M])(u: SumMapping[U[P], SupportedTargets])(using t: Target): Option[P]

type U[P] = P match 
  case PDT2[m] => m 

object PlatformDependentMapping2:
  inline def derived[P](using
      DummyImplicit
  ): PlatformDependentMapping2[P] = new PlatformDependentMapping2[P]:
    def withCT[A](fn: [T <: Target] => T ?=> ClassTag[U[P][T]] => A)(using t: Target): A = t.reveal[
      [T <: Target] =>> Tuple1[ClassTag[U[P][T]]]
    ](
      [T <: Target] => t ?=> tup => fn(tup._1)
    )

    def unspecific(u: SumMapping[U[P], SupportedTargets])(using t: Target): Option[P] = ???
    def unspecific3[M[_ <: Target] <: Matchable](using P <:< PDT2[M])(u: SumMapping[U[P], SupportedTargets])(using t: Target): Option[P] = ???

  def derive[P](using
      Quotes, Type[P]
  ): Expr[PlatformDependentMapping2[P]] =
    import quotes.reflect.*

    '{
      new PlatformDependentMapping2[P]:
        def withCT[A](
          fn: [T <: Target] => T ?=> ClassTag[U[P][T]] => A
        )(using t: Target): A = t.reveal[
          [T <: Target] =>> Tuple1[ClassTag[U[P][T]]],
        ](
          [T <: Target] => (t: T) ?=> tup => fn(tup._1)
        )

        def unspecific[M <: [_ <: Target] =>> Matchable](
            u: SumMapping[U[P], SupportedTargets]
        )(using t: Target): Option[P] = ???
        def unspecific3[M[_ <: Target] <: Matchable](using P <:< PDT2[M])(u: M[LinuxX64] | (M[WinX64] | (M[MacX64] | Nothing)))(using t: Target): Option[P] = ???
          // withCT([T <: Target] => t => (ct: ClassTag[M[T]]) ?=> 
          //   u match
          //     case b: M[T] => Some(new PDT2 {val value: Matchable = b}.asInstanceOf[P])
          //     case _ => None
          // )
    }