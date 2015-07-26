package org.pico.twiddle.syntax

import org.pico.twiddle.FixedInt

package object fixedInt {
  def bitSize[@specialized(Byte, Short, Int, Long) A: FixedInt]: Int = implicitly[FixedInt[A]].bitSize

  implicit class BitIntOps[A](val self: A) extends AnyVal {
    @inline final def hex(implicit ev: FixedInt[A]): String = ev.hex(self)

    @inline final def ubyte(implicit ev: FixedInt[A]): Byte = ev.ubyte(self)
    @inline final def ushort(implicit ev: FixedInt[A]): Short = ev.ushort(self)
    @inline final def uint(implicit ev: FixedInt[A]): Int = ev.uint(self)
    @inline final def ulong(implicit ev: FixedInt[A]): Long = ev.ulong(self)

    @inline final def >>>>(offset: Long)(implicit ev: FixedInt[A]): A = ev.>>>>(self, offset)
    @inline final def <<<<(offset: Long)(implicit ev: FixedInt[A]): A = ev.<<<<(self, offset)
    @inline final def ||||(that: A)(implicit ev: FixedInt[A]): A = ev.||||(self, that)
  }
}
