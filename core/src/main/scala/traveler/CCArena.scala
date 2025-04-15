package traveler

import language.experimental.captureChecking
import scala.caps.CapSet

trait CCArena: 
  def confinedSubArena[A](t: NThread)(fn: CCConfinedArena^{t} ?->{this} A^{this}): A^{this} = 
    val eh: CCConfinedArena^{t} = CCConfinedArena()
    val result = fn(using eh)
    result

  def sharedSubArena[A, B <: CapSet^{this}](fn: CCSharedArena^ ?->{this} A^{this}): A^{this} = 
    val eh: CCSharedArena^ = CCSharedArena()
    val result: A^{this} = fn(using eh)
    result
  
  def get: Object^{this} = Object()

class CCConfinedArena extends CCArena
class CCSharedArena extends CCArena
object GlobalArena extends CCArena

object CCArena:
  given global: CCArena = GlobalArena

  def confined[A](using a: CCArena^, t: NThread)(fn: CCConfinedArena^{t} ?->{a} A^{a}): A^{a} = a.confinedSubArena(t)(fn)
  def shared[A](using a: CCArena^)(fn: CCSharedArena^ ?->{a} A^{a}): A^{a} = 
    a.sharedSubArena(fn)

  def apply[A <: CCArena,B^](using a: A^{B^})(): A^{B^} = a


