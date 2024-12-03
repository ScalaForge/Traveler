package traveler

import traveler.Target.{WinX64, LinuxX64, MacX64}
import scala.compiletime.codeOf
import traveler.pdts.PDTNumeric.IntegralTypes
import traveler.pdts.PDT

@main def program =
  // println(Macro.myMac)

  val a = PDT[CLong].unspecific(5L)
  val b = PDT[CLong].unspecific(10)
  val c = PDT[CLong].unspecific(6L)
  // broken
  for
    readyA <- a
    readyB <- b
  do
    println("I shouldn't run!!")
    println(
      readyA + readyB
    )

  for
    readyA <- a
    readyC <- c
  do
    println(
      readyA + readyC
    )

  Target.assume[Target.LinuxX64] {
    val d = PDT[CLong](5L)
    val e = PDT[CLong](6L)

    println(d.unwrap)

    println(d + e)
    println("raw sum")
    val f = d.unwrap + e.unwrap
    println(f)
  }


  val g = PDT[CLong].fromMinima(5)
  val h = PDT[CLong].fromMinima(6)
  Target.assume[(Target.LinuxX64, Target.MacX64)] { 

    val i = PDT[CFoo].fromMinima(10: Short)

    println(g.unwrap + h.unwrap)
    println(i.toMaxima + g.toMaxima)
  }

  // val f: CLong = 'a'.asInstanceOf[CLong]

  // Target.assume[Target.LinuxX64] {
  //   f.unwrap
  // }

  println(Target.assume[Target.WinX64] {
    val d = CLong.inst(5)
    val e = CLong.inst(7)
    println("i shouldn't run!!")
    println(d + e)
  })

  println(PDT[CLong].fromMinima(2))

  // println(NumericMapping.create[M].fn(LinuxX64))

type M[T <: Target] <: IntegralTypes = T match
  case Target.LinuxX64 | Target.MacX64 => Int
  case Target.WinX64                   => Short

type M2[T <: Target] = Int

// val yyy: pdts.MappingMinima[M, Target.SupportedTargets, Double] = Int23(5).get
