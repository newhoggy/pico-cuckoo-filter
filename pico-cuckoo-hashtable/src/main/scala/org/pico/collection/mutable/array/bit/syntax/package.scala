package org.pico.collection.mutable.array.bit

import org.pico.twiddle.syntax._

package object syntax {
  implicit class LongOps_2s8EdpV(val self: Long) extends AnyVal {
    def >>>>(offset: Long): Long = {
      if (offset > 0) {
        self >>> offset
      } else {
        self << -offset
      }
    }

    def <<<<(offset: Long): Long = self << offset

    def ||||(that: Long): Long = self | that
  }

  implicit class IntOps_2s8EdpV(val self: Int) extends AnyVal {
    def >>>>(offset: Long): Int = {
      if (offset > 0) {
        self >>> offset.toInt
      } else {
        self << -offset.toInt
      }
    }

    def <<<<(offset: Long): Int = {
      if (offset > 0) {
        self <<   offset.toInt
      } else {
        self >>> -offset.toInt
      }
    }

    def ||||(that: Int): Int = self | that
  }

  implicit class ShortOps_2s8EdpV(val self: Short) extends AnyVal {
    def >>>>(offset: Long): Short = {
      if (offset > 0) {
        ((0xffff & self.toInt) >>> offset.toInt).toShort
      } else {
        ((0xffff & self.toInt) << -offset.toInt).toShort
      }
    }

    def <<<<(offset: Long): Short = {
      if (offset > 0) {
        ((0xffff & self.toInt) <<   offset.toInt).toShort
      } else {
        ((0xffff & self.toInt) >>> -offset.toInt).toShort
      }
    }

    def ||||(that: Short): Short = (self | that).toShort
  }

  implicit class ByteOps_2s8EdpV(val self: Byte) extends AnyVal {
    def >>>>(offset: Long): Byte = {
      if (offset > 0) {
        ((0xff & self.toInt) >>> offset.toInt).toByte
      } else {
        ((0xff & self.toInt) << -offset.toInt).toByte
      }
    }

    def <<<<(offset: Long): Byte = {
      if (offset > 0) {
        ((0xff & self.toInt) <<   offset.toInt).toByte
      } else {
        ((0xff & self.toInt) >>> -offset.toInt).toByte
      }
    }

    def ||||(that: Byte): Byte = (self | that).toByte
  }

  implicit class LongBitArrayOps_2s8EdpV(val array: Array[Long]) extends AnyVal {
    final def setAtIndex(i: Long, v: Long): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Long = array(i.toInt)

    final def long(i: Long, v: Put[Long]): Unit = {
      val b = i / 64
      val o = i % 64

      if (o == 0) {
        setAtIndex(b, v.value)
      } else {
        val n = 64 - o

        setAtIndex(b + 0, getAtIndex(b + 0) >>>> n <<<< n |||| v.value >>>> o)
        setAtIndex(b + 1, getAtIndex(b + 1) <<<< o >>>> o |||| v.value <<<< n)
      }
    }

    final def long(i: Long): Long = {
      val b = i / 64
      val o = i % 64

      if (o == 0) {
        getAtIndex(b)
      } else {
        val n = 64 - o

        getAtIndex(b + 0) << o |||| getAtIndex(b + 1) >>> n
      }
    }
  }

  implicit class IntBitArrayOps_2s8EdpV(val array: Array[Int]) extends AnyVal {
    final def setAtIndex(i: Long, v: Int): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Int = array(i.toInt)

    final def int(i: Long, v: Put[Int]): Unit = {
      val b = i / 32
      val o = i % 32

      if (o == 0) {
        setAtIndex(b, v.value)
      } else {
        val n = o - 32

        setAtIndex(b + 0, getAtIndex(b + 0) <<<< n >>>> n |||| v.value >>>> o)
        setAtIndex(b + 1, getAtIndex(b + 1) <<<< o >>>> o |||| v.value >>>> n)
      }
    }

    final def int(i: Long): Int = {
      val b = i / 32
      val o = i % 32

      if (o == 0) {
        getAtIndex(b)
      } else {
        val n = o - 32

        getAtIndex(b + 0) <<<< o |||| getAtIndex(b + 1) <<<< n
      }
    }
  }

  implicit class ShortBitArrayOps_2s8EdpV(val array: Array[Short]) extends AnyVal {
    final def setAtIndex(i: Long, v: Short): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Short = array(i.toInt)

    final def short(i: Long, v: Put[Short]): Unit = {
      val b = i / 16
      val o = i % 16

      if (o == 0) {
        setAtIndex(b, v.value)
      } else {
        val n = o - 16

        setAtIndex(b + 0, getAtIndex(b + 0) <<<< n >>>> n |||| v.value >>>> o)
        setAtIndex(b + 1, getAtIndex(b + 1) <<<< o >>>> o |||| v.value >>>> n)
      }
    }

    final def short(i: Long): Short = {
      val b = i / 16
      val o = i % 16

      if (o == 0) {
        getAtIndex(b)
      } else {
        val n = o - 16

        getAtIndex(b + 0) <<<< o |||| getAtIndex(b + 1) <<<< n
      }
    }
  }

  implicit class ByteBitArrayOps_2s8EdpV(val array: Array[Byte]) extends AnyVal {
    final def setAtIndex(i: Long, v: Byte): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Byte = array(i.toInt)

    final def byte(i: Long, v: Put[Byte]): Unit = {
      val b = i / 8
      val o = i % 8

      if (o == 0) {
        setAtIndex(b, v.value)
      } else {
        val n = o - 8

        setAtIndex(b + 0, getAtIndex(b + 0) <<<< n >>>> n |||| v.value >>>> o)
        setAtIndex(b + 1, getAtIndex(b + 1) <<<< o >>>> o |||| v.value >>>> n)
      }
    }

    final def byte(i: Long): Byte = {
      val b = i / 8
      val o = i % 8

      if (o == 0) {
        getAtIndex(b)
      } else {
        val n = o - 8

        getAtIndex(b + 0) <<<< o |||| getAtIndex(b + 1) <<<< n
      }
    }

    final def short(i: Long, v: Put[Short]): Unit = {
      val b = i / 8
      val o = i % 8

      val n = o - 8
      val p = o + 8

      setAtIndex(b + 0, getAtIndex(b + 0) <<<< n >>>> n |||| (v.value >>>> p).toByte)
      setAtIndex(b + 1,                                      (v.value >>>> o).toByte)
      setAtIndex(b + 2, getAtIndex(b + 2) <<<< o >>>> o |||| (v.value >>>> n).toByte)
    }


    final def short(i: Long): Short = {
      val b = i / 8
      val o = i % 8
      val n = o - 8
      val p = o + 8

      val aa = (getAtIndex(b + 0) & 0xff).toShort <<<< p
      val bb = (getAtIndex(b + 1) & 0xff).toShort <<<< o
      val cc = (getAtIndex(b + 2) & 0xff).toShort <<<< n

      aa |||| bb |||| cc
    }

    final def int(i: Long, v: Put[Int]): Unit = {
      val b = i / 8
      val o = i % 8
      val n = o - 8
      val p = o + 8
      val q = o + 16
      val r = o + 24

      println(s"int($i, $v: ${v.value.hex})")

      setAtIndex(b + 0, getAtIndex(b + 0) <<<< n >>>> n |||| (v.value >>>> r).toByte)
      setAtIndex(b + 1,                                      (v.value >>>> q).toByte)
      setAtIndex(b + 2,                                      (v.value >>>> p).toByte)
      setAtIndex(b + 3,                                      (v.value >>>> o).toByte)
      setAtIndex(b + 4, getAtIndex(b + 4) <<<< o >>>> o |||| (v.value >>>> n).toByte)
    }


    final def int(i: Long): Int = {
      val b = i / 8
      val o = i % 8
      val n = o - 8
      val p = o + 8
      val q = o + 16
      val r = o + 24

      val aa = (getAtIndex(b + 0).toInt & 0xff) <<<< r
      val bb = (getAtIndex(b + 1).toInt & 0xff) <<<< q
      val cc = (getAtIndex(b + 2).toInt & 0xff) <<<< p
      val dd = (getAtIndex(b + 3).toInt & 0xff) <<<< o
      val ee = (getAtIndex(b + 4).toInt & 0xff) <<<< n

      aa |||| bb |||| cc |||| dd |||| ee
    }
  }
}
