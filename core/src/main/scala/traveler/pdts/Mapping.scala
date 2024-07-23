package traveler.pdts

import traveler.Target
import traveler.pdts.NumericPDT.NumericTypes

//opaque type NumericID <: 0| 1 | 2 | 3 | 4 | 5 = 0 | 1 | 2 | 3 | 4 | 5

trait Mapping:
  type M[_ <: Target] <: Matchable 
  def apply(t: Target): Unit = ???


trait NumericMapping:
  type M[_ <: Target] <: NumericTypes
  def apply(t: Target): Int