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
import traveler.pdts.NumericTypes
import traveler.pdts.Int23

@main def program =
  val a = CLong.given_InstantiablePDT_Mapping_CLong.unspecific(5L)
  val b = CLong.given_InstantiablePDT_Mapping_CLong.unspecific(10)
  val c = CLong.given_InstantiablePDT_Mapping_CLong.unspecific(6L)
  // broken
  for
    readyA <- a
    readyB <- b
  do
    println("I shouldn't run!!")
    println(
      CLong.given_PDTNumeric_Mapping_CLong.add(readyA, readyB)
    )

  for
    readyA <- a
    readyC <- c
  do
    println(
      CLong.given_PDTNumeric_Mapping_CLong.add(readyA, readyC)
    )

  Target.assume[Target.LinuxX64] {
    val d = CLong.given_InstantiablePDT_Mapping_CLong(5L)
    val e = CLong.given_InstantiablePDT_Mapping_CLong(6L)
    println(CLong.given_PDTNumeric_Mapping_CLong.add(d, e))
    println("raw sum")
    println(
      CLong.given_InstantiablePDT_Mapping_CLong.unwrap(
        d
      ) + CLong.given_InstantiablePDT_Mapping_CLong.unwrap(e)
    )
  }

  println(Target.assume[Target.WinX64] {
    val d = CLong.given_InstantiablePDT_Mapping_CLong(5)
    val e = CLong.given_InstantiablePDT_Mapping_CLong(7)
    println("i shouldn't run!!")
    println(CLong.given_PDTNumeric_Mapping_CLong.add(d, e))
  })

  // println(NumericMapping.create[M].fn(LinuxX64))

type M[T <: Target] <: NumericTypes = T match
  case Target.LinuxX64 | Target.MacX64 => Int
  case Target.WinX64                   => Float

type M2[T <: Target] = Int

// val yyy: pdts.MappingMinima[M, Target.SupportedTargets, Double] = Int23(5).get
