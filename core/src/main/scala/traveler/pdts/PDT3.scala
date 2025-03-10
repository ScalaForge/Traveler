package traveler.pdts

import traveler.Target

opaque type Mapped[M <: [_ <: Target] =>> Matchable] = Matchable

object Mapped: 
  given [U, T <: Target, Fn <: [_ <: Target] =>> Matchable](using T, U =:= Fn[T]): Conversion[U, Mapped[Fn]] with 
    def apply(x: U): Mapped[Fn] = x.asInstanceOf[Fn[T]]