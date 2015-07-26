package org.pico.twiddle.syntax

import org.pico.twiddle.FixedInt2FixedInt

package object fixedInt2FixedInt {
  implicit class FixedInt2FixedIntOps[A](val self: A) extends AnyVal {
    def fixAs[B](implicit ev: FixedInt2FixedInt[A, B]): B = ev.fixAs(self)
  }
}
