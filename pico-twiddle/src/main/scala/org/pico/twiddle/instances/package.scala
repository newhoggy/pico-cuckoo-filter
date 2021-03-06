package org.pico.twiddle

import org.pico.twiddle.syntax.fixedInt._

package object instances {
  implicit val arrayIndexedByteArray = new ArrayIndexed[Array, Byte] {
    override def setAtIndex(indexed: Array[Byte], i: Bits, v: Byte): Unit = indexed(i.value.toInt) = v

    override def getAtIndex(indexed: Array[Byte], i: Bits): Byte = indexed(i.value.toInt)
  }

  implicit val arrayIndexedShortArray = new ArrayIndexed[Array, Short] {
    override def setAtIndex(indexed: Array[Short], i: Bits, v: Short): Unit = indexed(i.value.toInt) = v

    override def getAtIndex(indexed: Array[Short], i: Bits): Short = indexed(i.value.toInt)
  }

  implicit val arrayIndexedIntArray = new ArrayIndexed[Array, Int] {
    override def setAtIndex(indexed: Array[Int], i: Bits, v: Int): Unit = indexed(i.value.toInt) = v

    override def getAtIndex(indexed: Array[Int], i: Bits): Int = indexed(i.value.toInt)
  }

  implicit val arrayIndexedLongArray = new ArrayIndexed[Array, Long] {
    override def setAtIndex(indexed: Array[Long], i: Bits, v: Long): Unit = indexed(i.value.toInt) = v

    override def getAtIndex(indexed: Array[Long], i: Bits): Long = indexed(i.value.toInt)
  }

  implicit val fixedIntLong = new FixedInt[Long] {
    override def bitSize: Bits = Bits(64)

    override def hex(self: Long): String = (0L until bitSize.value).reverse.map(i => (self >> i) & 0x1).mkString("")

    override def ubyte(self: Long): Byte = self.toByte
    override def ushort(self: Long): Short = self.toShort
    override def uint(self: Long): Int = self.toInt
    override def ulong(self: Long): Long = self.toLong

    override def >>>>(self: Long, offset: Long): Long = {
      offset match {
        case o if o >= 64 => 0L
        case o if o > 0   => self >>> offset
        case o if o > -64 => self << -offset
        case _            => 0L
      }
    }

    override def <<<<(self: Long, offset: Long): Long = {
      offset match {
        case o if o >= 64 => 0L
        case o if o > 0   => self << offset
        case o if o > -64 => self >>> -offset
        case _            => 0L
      }
    }

    override def ||||(self: Long, that: Long): Long = self | that
  }

  implicit val fixedIntInt = new FixedInt[Int] {
    override def bitSize: Bits = Bits(32)

    override def hex(self: Int): String = (0L until bitSize.value).reverse.map(i => (self >> i) & 0x1).mkString("")

    override def ubyte(self: Int): Byte = self.toByte
    override def ushort(self: Int): Short = self.toShort
    override def uint(self: Int): Int = self
    override def ulong(self: Int): Long = 0xffffffffL & self.toLong

    override def >>>>(self: Int, offset: Long): Int = (ulong(self) >>>> offset).uint
    override def <<<<(self: Int, offset: Long): Int = (ulong(self) <<<< offset).uint
    override def ||||(self: Int, that: Int): Int = self | that
  }

  implicit val fixedIntShort = new FixedInt[Short] {
    override def bitSize: Bits = Bits(16)

    override def hex(self: Short): String = (0L until bitSize.value).reverse.map(i => (self >> i) & 0x1).mkString("")

    override def ubyte(self: Short): Byte = self.toByte
    override def ushort(self: Short): Short = self
    override def uint(self: Short): Int = 0xffff & self.toInt
    override def ulong(self: Short): Long = 0xffffL & self.toLong

    override def >>>>(self: Short, offset: Long): Short = (ulong(self) >>>> offset).ushort
    override def <<<<(self: Short, offset: Long): Short = (ulong(self) <<<< offset).ushort
    override def ||||(self: Short, that: Short): Short = (self | that).toShort
  }

  implicit lazy val fixedIntByte = new FixedInt[Byte] {
    override def bitSize: Bits = Bits(8)

    override def hex(self: Byte): String = (0L until bitSize.value).reverse.map(i => (self >> i) & 0x1).mkString("")

    override def ubyte(self: Byte): Byte = self.toByte
    override def ushort(self: Byte): Short = (0xff & self).toShort
    override def uint(self: Byte): Int = 0xff & self.toInt
    override def ulong(self: Byte): Long = 0xff & self.toLong

    override def >>>>(self: Byte, offset: Long): Byte = (ulong(self) >>>> offset).ubyte
    override def <<<<(self: Byte, offset: Long): Byte = (ulong(self) <<<< offset).ubyte
    override def ||||(self: Byte, that: Byte): Byte = (self | that).toByte
  }

  implicit val fixedInt2FixedInt_long_to_byte = new FixedInt2FixedInt[Long, Byte] {
    override def fixAs(self: Long): Byte = self.toByte
  }

  implicit val fixedInt2FixedInt_long_to_short = new FixedInt2FixedInt[Long, Short] {
    override def fixAs(self: Long): Short = self.toShort
  }

  implicit val fixedInt2FixedInt_long_to_int = new FixedInt2FixedInt[Long, Int] {
    override def fixAs(self: Long): Int = self.toInt
  }

  implicit val fixedInt2FixedInt_long_to_long = new FixedInt2FixedInt[Long, Long] {
    override def fixAs(self: Long): Long = self
  }

  implicit val orderedBits = new Ordering[Bits] {
    override def compare(x: Bits, y: Bits): Int = implicitly[Ordering[Long]].compare(x.value, y.value)
  }
}
