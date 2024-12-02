package traveler.pdts

import traveler.Target
import scala.compiletime.summonInline
import annotation.nowarn

trait PDTNumeric[
    P
]:
  def add(a: P, b: P): Target ?=> P
  def times(a: P, b: P): Target ?=> P

object PDTNumeric:
  type NumericTypes = Float | Double | IntegralTypes
  type IntegralTypes = Byte | Short | Int | Long

  @nowarn(
    "msg=New anonymous class definition will be duplicated at each inline site"
  )
  inline def derive[
      P,
      M[_ <: Target] <: PDTNumeric.IntegralTypes
  ](using _mapping: IntegralMapping[P, M], eqG: P =:= PDT) =
    new PDTNumeric[P]:
      def withContext[A](
          fn: [T <: Target] => (
              t: T
          ) ?=> Numeric[M[T]] => A
      )(using t: Target) =
        t.reveal[[T <: Target] =>> Tuple1[
          Numeric[M[T]]
        ], A](
          [T <: Target] => (t: T) ?=> tup => fn[T](tup._1)
        )

      def add(a: P, b: P): Target ?=> P =
        withContext(
          [T <: Target] =>
            T ?=>
              num => _mapping(num.plus(_mapping.unwrap(a), _mapping.unwrap(b)))
        )

      def times(a: P, b: P): Target ?=> P =
        withContext(
          [T <: Target] =>
            T ?=>
              num => _mapping(num.times(_mapping.unwrap(a), _mapping.unwrap(b)))
        )

  extension [P <: PDT](a: P)(using numeric: PDTNumeric[P])
    def +(b: P): P = numeric.add(a, b)
    def *(b: P): P = numeric.times(a, b)
