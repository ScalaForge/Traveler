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

  Target.platform match
    case given LinuxX64 =>
      val d = PDT[CLong](5L)
      val e = PDT[CLong](6L)

      CLong(5L)
      println(d.unwrap)

      println(d + e)
      println("raw sum")
      val f = d.unwrap + e.unwrap
      println(f)

  val g = PDT[CLong].fromMinima(5)
  val h = PDT[CLong].fromMinima(6)

  Target.platform match
    case given (LinuxX64 | MacX64) =>
      val i = PDT[CFoo].fromMinima(10: Short)

      println("pattern match here")
      println(g.unwrap + h.unwrap)
      println(i.toMaxima + g.toMaxima)

  // val f: CLong = 'a'.asInstanceOf[CLong]

  // Target.assume[Target.LinuxX64] {
  //   f.unwrap
  // }

  val x: Unit = Target.platform match
    case given Target.WinX64 =>
      val d = CLong.inst(5)
      val e = CLong.inst(7)
      println("i shouldn't run!!")
      println(d + e)
    case _ => ()

  println(PDT[CLong].fromMinima(2))

  PDT[CFoo].unspecific(5)
  PDT[CFoo].unspecific(5: Short)

  class A(using target: LinuxX64):
    val a: CFoo = PDT[CFoo](5: Short)
    // val b: CFoo = PDT[CFoo](5l) //doesn't compile

    val c: Short = a.unwrap

  val aa: CFoo = PDT[CFoo].fromMinima(5: Short)
  val ab: Long = aa.toMaxima

  // cannot compile because mapping doesn't resolve to a fixed value
  // val ba: Any = aa.unwrap

  val ca: CFoo = PDT[CFoo].fromMinima(10: Short)
  val cb: CFoo = PDT[CFoo].fromMinima(5: Short)
  val cc: CFoo = ca + cb

  val da: CLong = PDT[CLong].fromMinima(5)
  // cannot compile, types don't match
  // da + ca

  // only runs if the platform in question matches the target
  val signum: Option[Int] = Target.platform match
    case given LinuxX64 => Some(ca.unwrap.signum + cb.unwrap.signum)
    case _              => None // uses Short.signum

  // multiple targets can be used at once
  val myCalc: Option[Long] = Target.platform match
    case given (LinuxX64 | MacX64) => Some(ca.unwrap + da.unwrap)
    case _: WinX64                 => None

  val ea: Option[CFoo] = PDT[CFoo].unspecific(
    5
  ) // takes Int | Short, returns Some if current platform matches input
  val eb: CFoo = PDT[CFoo].fromMinima(5: Short)
  val ec: CFoo = Target.platform match
    case given (LinuxX64 | MacX64) => PDT[CFoo](5: Short)
    case given WinX64              => PDT[CFoo](5)
  val ed: Int = eb.toMaxima
  val ee: Int | Short = Target.platform match
    case given (LinuxX64 | MacX64) => eb.unwrap
    case given WinX64              => eb.unwrap

  // println(NumericMapping.create[M].fn(LinuxX64))

type M[T <: Target] <: IntegralTypes = T match
  case Target.LinuxX64 | Target.MacX64 => Int
  case Target.WinX64                   => Short

type M2[T <: Target] = Int

// val yyy: pdts.MappingMinima[M, Target.SupportedTargets, Double] = Int23(5).get
