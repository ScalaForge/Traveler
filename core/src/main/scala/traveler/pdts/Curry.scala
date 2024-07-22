package traveler.pdts

trait Curry[A]:
  type Out
  def apply(fn: A): Out

object Curry:
  sealed trait Ar1Fn extends Curry[Any => Any]:
    type Out = Any => Any 
    def apply(fn: Any => Any): Any => Any = fn 

  sealed trait Ar2Fn extends Curry[(Any, Any) => Any]:
    type Out = Any => Any => Any 
    def apply(fn: (Any, Any) => Any): Out = (a: Any) => (b: Any) => fn(a,b)