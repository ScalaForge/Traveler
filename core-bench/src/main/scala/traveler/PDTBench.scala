package traveler


import org.openjdk.jmh.annotations.*
import traveler.Target.LinuxX64
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.infra.Blackhole
import traveler.pdts.NumericPDT.NumericTypes
import traveler.pdts.NumericData
import traveler.pdts.NumericPDT

@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class PDTBench:
  given Target = LinuxX64
  val clong1 = CLong.given_InstantiablePDT_Mapping_CLong.unspecific(5l).get
  val clong2 = CLong.given_InstantiablePDT_Mapping_CLong.unspecific(10l).get

  val long1 = 5l 
  val long2 = 10l

  val clong3 = NumericData(5f)
  val clong4 = NumericData(10f)

  val clong5: CLongNumeric = {
    given LinuxX64.type = LinuxX64
    CLong.num(5)
  }

  @Benchmark
  def addSpecific(bh: Blackhole) =
    given LinuxX64.type = LinuxX64
    bh.consume(CLong.given_PDTNumeric_Mapping_CLong.addSpecific(clong1, clong2))

  @Benchmark
  def add(bh: Blackhole) = 
    bh.consume(CLong.given_PDTNumeric_Mapping_CLong.add(clong1, clong2))

  @Benchmark
  def addLong(bh: Blackhole) =
    bh.consume(Numeric[Long].plus(long1, long2))

  @Benchmark 
  def addNumericData(bh: Blackhole) = 
    bh.consume(
      NumericData(summon[Numeric[Float]].plus(clong3.reveal, clong4.reveal))
    )


  @Benchmark
  def mulSpecific = 
    given LinuxX64.type = LinuxX64
    CLong.given_PDTNumeric_Mapping_CLong.timesSpecific(clong1, clong2)

  @Benchmark
  def mul = 
    CLong.given_PDTNumeric_Mapping_CLong.times(clong1, clong2)

  @Benchmark 
  def mulLong = 
    Numeric[Long].times(long1, long2)

  @Benchmark
  def mulNumericData = 
    NumericData(summon[Numeric[Float]].times(clong3.reveal, clong4.reveal))

  

  
  


