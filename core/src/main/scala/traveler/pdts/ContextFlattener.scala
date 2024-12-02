package traveler.pdts
import scala.compiletime.erasedValue

type ContextFlattener[A] = A match
  case (a ?=> zz)      => A
  case ((a, b) ?=> zz) => a ?=> b ?=> zz

type ContextFlattener2[A, R] = A match
  case (a ?=> R)      => A
  case ((a, b) ?=> R) => a ?=> b ?=> R

type ContextFlattener3[A, Arity <: Int] = Arity match
  case 1 => A
  case 2 =>
    A match
      case ((a, b) ?=> zz) => a ?=> b ?=> zz

  case 3 =>
    A match
      case ((a, b, c) ?=> zz) => a ?=> b ?=> c ?=> zz

inline def flattenContext[A](inline fn: A): ContextFlattener[A] =
  inline fn match
    case _: (a ?=> zz)        => fn
    case bfn: ((a, b) ?=> zz) => (a: a) ?=> (b: b) ?=> bfn(using a, b)
