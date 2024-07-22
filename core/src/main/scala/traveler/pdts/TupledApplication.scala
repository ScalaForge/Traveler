package traveler.pdts

trait TupledApplication[A]:
  type InputTuple <: Tuple
  type Out
  def apply(inputTuple: InputTuple, fn: A): Out

object TupledApplication:
  object ar0fn extends TupledApplication[() => Any]:
    type InputTuple = EmptyTuple
    type Out = Any
    def apply(inputTuple: InputTuple, fn: () => Any): Out = fn()

  object ar1fn extends TupledApplication[Any => Any]:
    type InputTuple = Tuple1[Any]
    type Out = Any
    def apply(inputTuple: InputTuple, fn: Any => Any): Out = fn(inputTuple._1)

  object ar2fn extends TupledApplication[(Any, Any) => Any]:
    type InputTuple = (Any, Any)
    type Out = Any
    def apply(inputTuple: InputTuple, fn: (Any, Any) => Any): Out =
      fn(inputTuple._1, inputTuple._2)


  given ar0fnInst[ZZ]: (TupledApplication[() => ZZ] { 
    type InputTuple = EmptyTuple
    type Out = ZZ
  }) = ar0fn.asInstanceOf[TupledApplication[() => ZZ] {
    type InputTuple = EmptyTuple
    type Out = ZZ
  }]

  given ar1fnInst[A,ZZ]: (TupledApplication[A => ZZ] {
    type InputTuple = Tuple1[A]
    type Out = ZZ
  }) = ar1fn.asInstanceOf[TupledApplication[A => ZZ] {
    type InputTuple = Tuple1[A]
    type Out = ZZ
  }]