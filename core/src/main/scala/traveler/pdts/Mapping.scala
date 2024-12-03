package traveler.pdts

import traveler.Target
import traveler.Target.SupportedTargets
import scala.reflect.ClassTag
import scala.annotation.nowarn

trait Mapping[P, M[_ <: Target] <: Matchable]:
  type _M[T <: Target] = M[T]
  def unspecific(u: SumMapping[M, SupportedTargets])(using t: Target): Option[P]
  def apply[T <: Target](using t: T)(v: M[T]): P
  def unwrap[T <: Target](using t: T, ct: ClassTag[M[T]])(v: P): M[T]

object Mapping:
  @nowarn(
    "msg=New anonymous class definition will be duplicated at each inline site"
  )
  inline def derive[P, M[_ <: Target] <: Matchable](using
      conv: PDT =:= P
  ): Mapping[P, M] = new Mapping:
    def withCT[A](
        fn: [T <: Target] => T ?=> ClassTag[M[T]] => A
    )(using t: Target): A = t.reveal[
      [T <: Target] =>> Tuple1[ClassTag[M[T]]],
      A
    ](
      [T <: Target] => (t: T) ?=> tup => fn(tup._1)
    )

    def apply[T <: Target](using t: T)(v: M[T]): P = PDT.get(v)
    def unwrap[T <: Target](using t: T, ct: ClassTag[M[T]])(v: P): M[T] =
      PDT.unwrap(using t, ct, conv.flip)(v)
    def unspecific(
        u: SumMapping[M, SupportedTargets]
    )(using t: Target): Option[P] =
      withCT(
        [T <: Target] =>
          (t: T) ?=>
            ct =>
              given ClassTag[M[T]] = ct
              u match
                case b: M[T] => Some(PDT.get(b))
                case _       => None
      )
