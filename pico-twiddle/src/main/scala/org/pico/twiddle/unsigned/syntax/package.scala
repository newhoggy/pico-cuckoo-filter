package org.pico.twiddle.unsigned

import org.pico.twiddle.Bits

package object syntax {
  implicit class ByteOps_Ghg39HP(val self: Byte) extends AnyVal {
    def hex: String = (0 until 8).reverse.map(i => (self >> i) & 0x1).mkString("")

    def ubyte: Byte = self.toByte
    def ushort: Short = (0xff & self.toInt).toShort
    def uint: Int = 0xff & self.toInt
    def ulong: Long = 0xff & self.toLong

    def >>>>(offset: Bits): Byte = (ulong >>>> offset).ubyte
    def <<<<(offset: Bits): Byte = (ulong <<<< offset).ubyte

    def ||||(that: Byte): Byte = (self | that).toByte
  }

  implicit class ShortOps_Ghg39HP(val self: Short) extends AnyVal {
    def hex: String = (0 until 16).reverse.map(i => (self >> i) & 0x1).mkString("")

    def ubyte: Byte = self.toByte
    def ushort: Short = self
    def uint: Int = 0xffff & self.toInt
    def ulong: Long = 0xffff & self.toLong

    def >>>>(offset: Bits): Short = (ulong >>>> offset).ushort
    def <<<<(offset: Bits): Short = (ulong <<<< offset).ushort

    def ||||(that: Short): Short = (self | that).toShort
  }

  implicit class IntOps_Ghg39HP(val self: Int) extends AnyVal {
    def hex: String = (0 until 32).reverse.map(i => (self >> i) & 0x1).mkString("")

    def ushort: Short = self.toShort
    def uint: Int = self
    def ulong: Long = 0xffffffffL & self.toLong

    def >>>>(offset: Bits): Int = (ulong >>>> offset).uint
    def <<<<(offset: Bits): Int = (ulong <<<< offset).uint

    def ||||(that: Int): Int = self | that
  }

  implicit class LongOps_Ghg39HP(val self: Long) extends AnyVal {
    def hex: String = (0L until 64L).reverse.map(i => (self >> i) & 0x1).mkString("")

    def ubyte: Byte = self.toByte
    def ushort: Short = self.toShort
    def uint: Int = self.toInt
    def ulong: Long = self

    def >>>>(offset: Bits): Long = {
      offset.value match {
        case o if o >=  64  => 0L
        case o if o >   0   => self >>> offset.value
        case o if o >   -64 => self << -offset.value
        case _              => 0L
      }
    }

    def <<<<(offset: Bits): Long = {
      offset.value match {
        case o if o >=  64  => 0L
        case o if o >   0   => self << offset.value
        case o if o >   -64 => self >>> -offset.value
        case _              => 0L
      }
    }

    def ||||(that: Long): Long = self | that
  }
}
