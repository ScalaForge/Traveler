package traveler

import scala.language.experimental.captureChecking

class Monad[A](a: A):
  def flatMap[B](fa: A -> Monad[B]): Monad[B] = fa(a)
  def map[B](fa: A -> B): Monad[B] = Monad.pure(fa(a))
object Monad:
  def pure[A](a: A): Monad[A] = Monad(a)
