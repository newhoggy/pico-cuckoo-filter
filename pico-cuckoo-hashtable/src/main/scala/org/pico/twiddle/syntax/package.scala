package org.pico.twiddle

package object syntax {
  implicit class ByteOps_Ghg39HP(val self: Byte) extends AnyVal {
    def hex: String = (0 until 8).reverse.map(i => (self >> i) & 0x1).mkString("")

    def uint: Int = 0xff & self.toInt

    def >>>>(offset: Long): Byte = if (offset > 0) (uint >>> offset).toByte else (uint <<  -offset).toByte
    def <<<<(offset: Long): Byte = if (offset > 0) (uint <<  offset).toByte else (uint >>> -offset).toByte

    def ||||(that: Byte): Byte = (self | that).toByte
  }

  implicit class ShortOps_Ghg39HP(val self: Short) extends AnyVal {
    def hex: String = (0 until 16).reverse.map(i => (self >> i) & 0x1).mkString("")

    def uint: Int = 0xffff & self.toInt

    def >>>>(offset: Long): Short = if (offset > 0) (uint >>> offset).toShort else (uint <<  -offset).toShort
    def <<<<(offset: Long): Short = if (offset > 0) (uint <<  offset).toShort else (uint >>> -offset).toShort

    def ||||(that: Short): Short = (self | that).toShort
  }

  implicit class IntOps_Ghg39HP(val self: Int) extends AnyVal {
    def hex: String = (0 until 32).reverse.map(i => (self >> i) & 0x1).mkString("")

    def uint: Int = self

    def >>>>(offset: Long): Int = if (offset > 0) self >>> offset.toInt else self <<  -offset.toInt
    def <<<<(offset: Long): Int = if (offset > 0) self <<  offset.toInt else self >>> -offset.toInt

    def ||||(that: Int): Int = self | that
  }

  implicit class LongOps_Ghg39HP(val self: Long) extends AnyVal {
    def hex: String = (0L until 64L).reverse.map(i => (self >> i) & 0x1).mkString("")

    def uint: Int = self.toInt

    def >>>>(offset: Long): Long = if (offset > 0) self >>> offset else self <<  -offset
    def <<<<(offset: Long): Long = if (offset > 0) self <<  offset else self >>> -offset

    def ||||(that: Long): Long = self | that
  }
}
