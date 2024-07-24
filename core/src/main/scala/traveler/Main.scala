package traveler

import traveler.Target.{WinX64, LinuxX64, MacX64}
import traveler.pdts.flattenContext
import traveler.pdts.ContextFlattener
import traveler.pdts.ContextFlattener3
import traveler.pdts.Arity
import traveler.Target.Assumption
import scala.compiletime.codeOf
import traveler.pdts.NumericMapping
import traveler.pdts.NumericPDT

@main def program =
  given Target = LinuxX64
  println(CLong.given_InstantiablePDT_Mapping_CLong.unspecific(2.0f))

  {
    given WinX64.type = WinX64

    println(CLong.given_InstantiablePDT_Mapping_CLong(5.0f))
  }

  // val cfn: (Int, Float) ?=> String =

  type cfn = (Int, Float) ?=> String
  type fcfn = ContextFlattener[cfn]

  // summon[fcfn =:= (Int ?=> Float ?=> String)]

  val fcfn: ContextFlattener3[(Int, Float, String) ?=> String, 3] = (i: Int) ?=>
    (f: Float) ?=> (s: String) ?=> ((i + f).toString() + s)

  println(fcfn(using 1)(using 2)(using "hello"))

  println(summon[Arity[Int => Float]].out)

  Target.assume[LinuxX64.type].apply {
    println(CLong.given_InstantiablePDT_Mapping_CLong(5L))
    println(
      CLong.given_PDTNumeric_Mapping_CLong.add(
        CLong.given_InstantiablePDT_Mapping_CLong(5L),
        CLong.given_InstantiablePDT_Mapping_CLong(10L)
      )
    )

    println(
      codeOf {
        CLong.given_PDTNumeric_Mapping_CLong
          .addSpecific(
            CLong.given_InstantiablePDT_Mapping_CLong(5L),
            CLong.given_InstantiablePDT_Mapping_CLong(20L)
          )
      }
    )

    println(5f)
    println(java.lang.Float.intBitsToFloat(java.lang.Float.floatToRawIntBits(5.5)))
  }


type M[T <: Target] <: NumericPDT.NumericTypes = T match 
  case LinuxX64.type | MacX64.type => Int
  case WinX64.type => Float

val nm = NumericMapping.create[M]