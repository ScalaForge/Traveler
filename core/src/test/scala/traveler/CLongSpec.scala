package traveler

class CLongSpec:
  // println(Macro.myMac)

  val a = PDT.unspecific[CLong](5L)
  val b = PDT.unspecific[CLong](10)
  val c = PDT.unspecific[CLong](6L)
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
    case t @ given LinuxX64 =>
      val d = CLong(5L)
      val e = CLong(6L)

      CLong(5L)
      println(d.unwrap)

      println(d +& e)
      println("raw sum")
      val f = d +& e
      println(f)

  val g = PDT.fromMinima[CLong](5)
  val h = PDT.fromMinima[CLong](6)

  Target.platform match
    case given (LinuxX64 | MacX64) =>
      val i = PDT.fromMinima[CFoo](10: Short)

      println("pattern match here")
      println(g.unwrap + h.unwrap)
      println(i.toMaxima + g.toMaxima)

  val f: CLong = 'a'.asInstanceOf[CLong]

  // Target.assume[Target.LinuxX64] {
  //   f.unwrap
  // }

  val x: Unit = Target.platform match
    case given Target.WinX64 =>
      val d = CLong(5)
      val e = CLong(6)
      println("i shouldn't run!!")
      println(d + e)
    case _ => ()

  println(PDT.fromMinima[CLong](2))

  CFoo.unspecific(5)
  PDT.unspecific[CFoo](5)
  PDT.unspecific[CFoo](5: Short)


  val aa: CFoo = CFoo.fromMinima(5: Short)
  val ab: Long = aa.toMaxima

  // cannot compile because mapping doesn't resolve to a fixed value
  // val ba: Any = aa.unwrap

  val ca: CFoo = PDT.fromMinima[CFoo](10: Short)
  val cb: CFoo = PDT.fromMinima[CFoo](5: Short)
  val cc: CFoo = ca + cb

  val da: CLong = PDT.fromMinima[CLong](5)
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

  val ea: Option[CFoo] = PDT.unspecific[CFoo](
    5
  ) // takes Int | Short, returns Some if current platform matches input
  val eb: CFoo = PDT.fromMinima[CFoo](5: Short)
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

