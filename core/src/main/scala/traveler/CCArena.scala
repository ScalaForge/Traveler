package traveler

import language.experimental.captureChecking
import scala.caps.CapSet
import java.lang.foreign.Arena
import java.lang.foreign.FunctionDescriptor
import java.lang.invoke.MethodHandle
import java.lang.foreign.Linker
import java.nio.file.Path
import java.lang.foreign.SymbolLookup
import java.lang.foreign.StructLayout
import scala.collection.mutable

trait CCArena(a: Arena): 
  protected val symbolLoaderForLib: collection.mutable.Map[Path, SymbolLookup]
  protected val methodHandleCache: collection.mutable.Map[String, MethodHandle]
  def confinedSubArena[A](t: NThread)(fn: CCConfinedArena^{t} ?->{this} A^{this}): A^{this} = 
    val eh: CCConfinedArena^{t} = CCConfinedArena()
    val result = fn(using eh)
    result

  def sharedSubArena[A](fn: CCSharedArena^ ?->{this} A^{this}): A^{this} = 
    val eh: CCSharedArena^ = new CCSharedArena()
    val result: A^{this} = fn(using eh)
    result
  
  def get: Object^{this} = Object()
  private def createLookup(path: Path): SymbolLookup = symbolLoaderForLib.getOrElseUpdate(path, SymbolLookup.libraryLookup(path, a))
  def getMethodHandle(symbolName: String, libFile: Path, functionDescription: FunctionDescriptor)(using Linker): MethodHandle^{this} = 
    val sl = createLookup(libFile)
    methodHandleCache.getOrElseUpdate(symbolName, {
      val mh = summon[Linker].downcallHandle(sl.findOrThrow(symbolName), functionDescription)
      val isStackStruct = functionDescription.returnLayout.map(_ match 
        case _: StructLayout => true 
        case _ => false).orElseGet(() => false)
      if isStackStruct then mh.bindTo(a)
      else mh
    }
    )
  def close(): Unit = a.close()

class CCConfinedArena extends CCArena(Arena.ofConfined()): 
  override protected val methodHandleCache: mutable.Map[String, MethodHandle] = 
    collection.mutable.HashMap()
  override protected val symbolLoaderForLib: mutable.Map[Path, SymbolLookup] = 
    collection.mutable.HashMap()
class CCSharedArena extends CCArena(Arena.ofShared()):
  override protected val methodHandleCache: mutable.Map[String, MethodHandle] = 
    collection.concurrent.TrieMap()
  override protected val symbolLoaderForLib: mutable.Map[Path, SymbolLookup] = 
    collection.concurrent.TrieMap()

object CCSharedArena:
  def apply[B^](using a: CCSharedArena^{B^})(): CCSharedArena^{B^} = a

object GlobalArena extends CCArena(Arena.global()):
  override protected val methodHandleCache: mutable.Map[String, MethodHandle] = 
    collection.concurrent.TrieMap()
  override protected val symbolLoaderForLib: mutable.Map[Path, SymbolLookup] = 
    collection.concurrent.TrieMap()

object CCArena:
  given global: CCArena = GlobalArena

  def confined[A](using a: CCArena^, t: NThread)(fn: CCConfinedArena^{t} ?->{a} A^{a}): A^{a} = a.confinedSubArena(t)(fn)
  def shared[A](using a: CCArena^)(fn: CCSharedArena^ ?->{a} A^{a}): A^{a} = 
    a.sharedSubArena(fn)

  def apply[B^](using a: CCArena^{B^})(): CCArena^{B^} = a
