package traveler


import org.openjdk.jmh.annotations.*
import traveler.Target.LinuxX64
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.infra.Blackhole
import traveler.pdts.PDT
import traveler.pdts.PDTNumeric
import traveler.pdts.Test
import scala.annotation.experimental

@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class PDTBench:
  given Target = LinuxX64
  val clong1 = PDT.unspecific[CLong](5l).get
  val clong2 = PDT.unspecific[CLong](10l).get

  val long1 = 5l 
  val long2 = 10l


  @Benchmark
  def addSpecific(bh: Blackhole) =
    given LinuxX64 = LinuxX64
    bh.consume(clong1.unwrap + clong2.unwrap)

  @Benchmark
  def add(bh: Blackhole) = 
    bh.consume(clong1 + clong2)

  @Benchmark
  def addLong(bh: Blackhole) =
    bh.consume(Numeric[Long].plus(long1, long2))


  @Benchmark
  def mulSpecific(bh: Blackhole) = 
    given LinuxX64 = LinuxX64
    bh.consume(clong1.unwrap * clong2.unwrap)

  @Benchmark
  def mul(bh: Blackhole) = 
    bh.consume(clong1 * clong2)

  @Benchmark 
  def mulLong = 
    Numeric[Long].times(long1, long2)

  @Benchmark
  def unspecific1 = 
    PDT.unspecific[CLong](5)

  @Benchmark
  @experimental
  def unspecific2 = 
    Test.CBar.derived$PlatformDependentMapping2.unspecific(5)
