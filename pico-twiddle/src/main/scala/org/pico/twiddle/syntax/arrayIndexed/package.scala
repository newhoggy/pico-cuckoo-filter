package org.pico.twiddle.syntax

import org.pico.twiddle.ArrayIndexed

import scala.language.higherKinds

package object arrayIndexed {
  implicit class ArrayIndexedOps[F[_], E](val self: F[E]) extends AnyVal {
    def setAtIndex(
        i: Long, e: E)(implicit ev: ArrayIndexed[F, E]): Unit = ev.setAtIndex(self, i, e)

    def getAtIndex(
        i: Long)(implicit ev: ArrayIndexed[F, E]): E = ev.getAtIndex(self, i)
  }
}
