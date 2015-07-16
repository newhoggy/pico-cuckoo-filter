package org.pico.collection.mutable.array.bit

package object syntax {
  implicit class LongOps_2s8EdpV(val self: Long) extends AnyVal {
    def >>>>(offset: Long): Long = self >>> offset

    def <<<<(offset: Long): Long = self << offset

    def ||||(that: Long): Long = self | that
  }

  implicit class IntOps_2s8EdpV(val self: Int) extends AnyVal {
    def >>>>(offset: Long): Int = self >>> offset.toInt

    def <<<<(offset: Long): Int = self << offset.toInt

    def ||||(that: Int): Int = self | that
  }

  implicit class ShortOps_2s8EdpV(val self: Short) extends AnyVal {
    def >>>>(offset: Long): Short = ((0xffff & self.toInt) >>> offset.toInt).toShort

    def <<<<(offset: Long): Short = ((0xffff & self.toInt) << offset.toInt).toShort

    def ||||(that: Short): Short = (self | that).toShort
  }

  implicit class ByteOps_2s8EdpV(val self: Byte) extends AnyVal {
    def >>>>(offset: Long): Byte = ((0xff & self.toInt) >>> offset.toInt).toByte

    def <<<<(offset: Long): Byte = ((0xff & self.toInt) << offset.toInt).toByte

    def ||||(that: Byte): Byte = (self | that).toByte
  }

  implicit class LongBitArrayOps_2s8EdpV(val array: Array[Long]) extends AnyVal {
    final def setAtIndex(i: Long, v: Long): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Long = array(i.toInt)

    final def long(n: Long, v: Put[Long]): Unit = {
      val i = n / 64
      val o = n % 64

      if (o == 0) {
        setAtIndex(i, v.value)
      } else {
        val p = 64 - o

        setAtIndex(i + 0, getAtIndex(i + 0) >>>> p <<<< p |||| v.value >>>> o)
        setAtIndex(i + 1, getAtIndex(i + 1) <<<< o >>>> o |||| v.value <<<< p)
      }
    }

    final def long(n: Long): Long = {
      val i = n / 64
      val o = n % 64

      if (o == 0) {
        getAtIndex(i)
      } else {
        val p = 64 - o

        getAtIndex(i + 0) << o |||| getAtIndex(i + 1) >>> p
      }
    }
  }

  implicit class IntBitArrayOps_2s8EdpV(val array: Array[Int]) extends AnyVal {
    final def setAtIndex(i: Long, v: Int): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Int = array(i.toInt)

    final def int(n: Long, v: Put[Int]): Unit = {
      val i = n / 32
      val o = n % 32

      if (o == 0) {
        setAtIndex(i, v.value)
      } else {
        val p = 32 - o

        setAtIndex(i + 0, getAtIndex(i + 0) >>>> p <<<< p |||| v.value >>>> o)
        setAtIndex(i + 1, getAtIndex(i + 1) <<<< o >>>> o |||| v.value <<<< p)
      }
    }

    final def int(n: Long): Int = {
      val i = n / 32
      val o = n % 32

      if (o == 0) {
        getAtIndex(i)
      } else {
        val p = 32 - o

        getAtIndex(i + 0) <<<< o |||| getAtIndex(i + 1) >>>> p
      }
    }
  }

  implicit class ShortBitArrayOps_2s8EdpV(val array: Array[Short]) extends AnyVal {
    final def setAtIndex(i: Long, v: Short): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Short = array(i.toInt)

    final def short(n: Long, v: Put[Short]): Unit = {
      val i = n / 16
      val o = n % 16

      if (o == 0) {
        setAtIndex(i, v.value)
      } else {
        val p = 16 - o

        setAtIndex(i + 0, getAtIndex(i + 0) >>>> p <<<< p |||| v.value >>>> o)
        setAtIndex(i + 1, getAtIndex(i + 1) <<<< o >>>> o |||| v.value <<<< p)
      }
    }

    final def short(n: Long): Short = {
      val i = n / 16
      val o = n % 16

      if (o == 0) {
        getAtIndex(i)
      } else {
        val p = 16 - o

        getAtIndex(i + 0) <<<< o |||| getAtIndex(i + 1) >>>> p
      }
    }
  }

  implicit class ByteBitArrayOps_2s8EdpV(val array: Array[Byte]) extends AnyVal {
    final def setAtIndex(i: Long, v: Byte): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Byte = array(i.toInt)

    final def byte(n: Long, v: Put[Byte]): Unit = {
      val i = n / 8
      val o = n % 8

      if (o == 0) {
        setAtIndex(i, v.value)
      } else {
        val p = 8 - o

        setAtIndex(i + 0, getAtIndex(i + 0) >>>> p <<<< p |||| v.value >>>> o)
        setAtIndex(i + 1, getAtIndex(i + 1) <<<< o >>>> o |||| v.value <<<< p)
      }
    }

    final def byte(n: Long): Byte = {
      val i = n / 8
      val o = n % 8

      if (o == 0) {
        getAtIndex(i)
      } else {
        val p = 8 - o

        getAtIndex(i + 0) <<<< o |||| getAtIndex(i + 1) >>>> p
      }
    }
  }
}
