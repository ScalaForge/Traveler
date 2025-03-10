package traveler

import traveler.Target.LinuxX64
import traveler.Target.MacX64
import traveler.Target.WinX64

trait SpecificTarget[T <: Target]

object SpecificTarget:
  given linuxX64Evidence: SpecificTarget[LinuxX64] =
    new SpecificTarget[LinuxX64] {}

  given macX64Evidence: SpecificTarget[MacX64] = 
    new SpecificTarget[MacX64] {}
  
  given winX64Evidence: SpecificTarget[WinX64] = 
    new SpecificTarget[WinX64] {}
