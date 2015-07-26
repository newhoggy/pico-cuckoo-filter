package org.pico.twiddle

import scala.language.higherKinds

trait ArrayIndexed[F[_], @specialized(Byte, Short, Int, Long) E] {
  def setAtIndex(indexed: F[E], i: Long, v: E): Unit

  def getAtIndex(indexed: F[E], i: Long): E
}
