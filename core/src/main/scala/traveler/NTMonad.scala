package traveler

import scala.language.experimental.captureChecking

opaque type NTMonad[A] = A

object NTMonad:
  def pure[A](a: A): NTMonad[A] = a
  extension [A](a: NTMonad[A])
    inline def map[B](inline fn: A -> B): NTMonad[B] = fn(a)
    inline def flatMap[B](inline fn: A -> NTMonad[B]): NTMonad[B] = fn(a)
