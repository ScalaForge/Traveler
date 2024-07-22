package traveler.pdts

trait Inputs[A]:
  type Out <: Tuple

object Inputs:
  private val ar1FnInputs: Inputs[Any => Any] { type Out = Tuple1[Any] } =
    new Inputs[Any => Any]:
      type Out = Tuple1[Any]

  private val ar2FnInputs: Inputs[(Any, Any) => Any] { type Out = (Any, Any) } =
    new Inputs[(Any, Any) => Any]:
      type Out = (Any, Any)

  given ar1FnInputs[A, ZZ]: (Inputs[A => ZZ] { type Out = Tuple1[A] }) =
    ar1FnInputs.asInstanceOf[Inputs[A => ZZ] { type Out = Tuple1[A] }]

  given ar2FnInputs[A, B, ZZ]: (Inputs[(A, B) => ZZ] { type Out = (A, B) }) = 
    ar2FnInputs.asInstanceOf[Inputs[(A,B) => ZZ] { type Out = (A, B)}]