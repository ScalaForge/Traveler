package traveler.pdts

trait Output[A]:
  type Out

object Output:
  private val base: Output[Any] { type Out = Any } =
    new Output[Any]:
      type Out = Any

  given ar1FnOutput[A, ZZ]: (Output[A => ZZ] { type Out = ZZ }) =
    base.asInstanceOf[Output[A => ZZ] { type Out = ZZ }]

  given ar2FnOutput[A, B, ZZ]: (Output[(A, B) => ZZ] { type Out = ZZ }) =
    base.asInstanceOf[Output[(A, B) => ZZ] { type Out = ZZ }]
