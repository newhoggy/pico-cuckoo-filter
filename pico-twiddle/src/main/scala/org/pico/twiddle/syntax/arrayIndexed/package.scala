package org.pico.twiddle.syntax

import org.pico.twiddle.ArrayIndexed

package object arrayIndexed {
  implicit class ArrayIndexedOps[A](val self: A) extends AnyVal {
    def setAtIndex[@specialized(Byte, Short, Int, Long) E](
        i: Long, e: E)(implicit ev: ArrayIndexed[A, E]): Unit = ev.setAtIndex(self, i, e)

    def getAtIndex[@specialized(Byte, Short, Int, Long) E](
        i: Long)(implicit ev: ArrayIndexed[A, E]): E = ev.getAtIndex(self, i)
  }
}
