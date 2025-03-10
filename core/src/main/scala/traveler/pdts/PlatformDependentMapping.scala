package traveler.pdts

import traveler.Target
import traveler.Target.SupportedTargets
import scala.reflect.ClassTag
import scala.annotation.nowarn
import scala.compiletime.summonInline

trait PlatformDependentMapping[P, M[_ <: Target] <: Matchable]:
  type _M[T <: Target] = M[T]
  def unspecific(u: SumMapping[M, SupportedTargets])(using t: Target): Option[P]
  def apply[T <: Target](using t: T)(v: M[T]): P
  def unwrap[T <: Target](using t: T)(v: P): M[T]


object PlatformDependentMapping:
  @nowarn(
    "msg=New anonymous class definition will be duplicated at each inline site"
  )
  inline def derive[P, M[_ <: Target] <: Matchable]: PlatformDependentMapping[P, M] = new PlatformDependentMapping:
    def withCT[A](
        fn: [T <: Target] => T ?=> ClassTag[M[T]] => A
    )(using t: Target): A = t.reveal[
      [T <: Target] =>> Tuple1[ClassTag[M[T]]],
    ](
      [T <: Target] => (t: T) ?=> tup => fn(tup._1)
    )

    def apply[T <: Target](using t: T)(v: M[T]): P = v.asInstanceOf[P]
    def unwrap[T <: Target](using t: T)(v: P): M[T] =
      v.asInstanceOf[M[T]]
    def unspecific(
        u: SumMapping[M, SupportedTargets]
    )(using t: Target): Option[P] =
      t.reveal[
        [T <: Target] =>> Tuple1[ClassTag[M[T]]],
      ](
        [T <: Target] =>
          t ?=>
            ct =>
              given ClassTag[M[T]] = ct._1
              u match
                case b: M[T] => Some(b.asInstanceOf[P])
                case _       => None
      )
