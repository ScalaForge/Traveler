package traveler.pdts

trait Fn[Tup <: Tuple, Output]:
  type Out

object Fn:
  val base = new Fn[EmptyTuple, Any]:
    type Out = Any

  given ar1[A, ZZ]: (Fn[Tuple1[A], ZZ] { type Out = A => ZZ }) =
    base.asInstanceOf[Fn[Tuple1[A], ZZ] { type Out = A => ZZ }]

  given ar2[A,B,ZZ]: (Fn[(A,B), ZZ]{ type Out = (A,B) => ZZ}) = 
    base.asInstanceOf[Fn[(A,B), ZZ]{ type Out = (A,B) => ZZ }]
