package traveler

import language.experimental.captureChecking
import scala.caps.Capability

class NThread extends Capability

object NThread:
  given NThread with {}

  def apply[A](fn: NThread ?-> A): A = fn(using new NThread())
  def inner[A](using eh: CCSharedArena^)(fn: NThread ?->{eh} A^{eh}): A^{eh} =
    fn(using new NThread())
