package traveler.pdts

opaque type Int23 <: Int = Int 

object Int23:
  def apply(i: Int): Option[Int23] = if i <= 8388608 || i >= -8388608 then Some(i) else None 
