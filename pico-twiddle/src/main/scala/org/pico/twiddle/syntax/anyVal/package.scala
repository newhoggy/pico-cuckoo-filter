package org.pico.twiddle.syntax

package object anyVal {
  implicit class ByteOps_2s8EdpV(val self: Byte) extends AnyVal {
    def byteSize = 1
    def bitSize = 8
  }

  implicit class ShortOps_2s8EdpV(val self: Short) extends AnyVal {
    def byteSize = 2
    def bitSize = 16
  }

  implicit class IntOps_2s8EdpV(val self: Int) extends AnyVal {
    def byteSize = 4
    def bitSize = 32
  }

  implicit class LongOps_2s8EdpV(val self: Long) extends AnyVal {
    def byteSize = 8
    def bitSize = 64
  }
}
