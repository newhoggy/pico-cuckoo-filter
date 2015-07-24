package org.pico.twiddle.syntax

package object anyVal {
  implicit class ByteOps_2s8EdpV(val self: Byte) extends AnyVal {
    @inline final def byteSize = 1
    @inline final def bitSize = 8
  }

  implicit class ShortOps_2s8EdpV(val self: Short) extends AnyVal {
    @inline final def byteSize = 2
    @inline final def bitSize = 16
  }

  implicit class IntOps_2s8EdpV(val self: Int) extends AnyVal {
    @inline final def byteSize = 4
    @inline final def bitSize = 32
  }

  implicit class LongOps_2s8EdpV(val self: Long) extends AnyVal {
    @inline final def byteSize = 8
    @inline final def bitSize = 64
  }
}
