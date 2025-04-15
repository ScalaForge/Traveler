package traveler

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class MonadBench {
  val objMonad = Monad.pure(5)
  val ntMonad = NTMonad.pure(5)
  @Benchmark
  def objectMonad(bh: Blackhole) =
    val res = for
      a <- objMonad
      b <- objMonad
      c = a + b
    yield c * a
    bh.consume(res)

  @Benchmark
  def newtypeMonad(bh: Blackhole) =
    val res = for
      a <- ntMonad
      b <- ntMonad
      c = a + b
    yield c * a
    bh.consume(res)
}
