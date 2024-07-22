package traveler.pdts

trait Arity[A]:
  type Out <: Singleton & Int
  val out: Out

object Arity:
  val ar1: Arity[Any] { type Out = 1 } = new Arity[Any]:
    type Out = 1
    val out: Out = 1

  val ar2: Arity[Any] { type Out = 2 } = new Arity[Any]:
    type Out = 2
    val out: Out = 2

  given ar1Fn[A, ZZ]: (Arity[A => ZZ] { type Out = 1 }) =
    ar1.asInstanceOf[Arity[A => ZZ] { type Out = 1 }]

  given ar2Fn[A, B, ZZ]: (Arity[(A, B) => ZZ] { type Out = 2 }) =
    ar2.asInstanceOf[Arity[(A, B) => ZZ] { type Out = 2 }]
