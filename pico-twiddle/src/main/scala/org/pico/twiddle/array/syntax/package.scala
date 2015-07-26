package org.pico.twiddle.array

import org.pico.twiddle.unsigned.syntax._
import org.pico.twiddle.syntax.anyVal._

import scala.annotation.tailrec

package object syntax {
  implicit class LongBitArrayOps_2s8EdpV(val array: Array[Long]) extends AnyVal {
    @inline final def elemBitSize: Int = 64

    final def setAtIndex(i: Long, v: Long): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Long = array(i.toInt)

    final def byte(i: Long, v: Byte): Unit = update(i, 8, v.ulong)
    final def short(i: Long, v: Short): Unit = update(i, 16, v.ulong)
    final def int(i: Long, v: Int): Unit = update(i, 32, v.ulong)
    final def long(i: Long, v: Long): Unit = update(i, 64, v.ulong)

    final def byte(i: Long): Byte = signed(i, 8).ubyte

    final def short(i: Long): Short = {
      val ai = i / elemBitSize
      val bi = ai + 1
      val o = i % elemBitSize
      val ars = o + 8
      val brs = (o - 8) max 0
      val aw = getAtIndex(ai)
      val bw = getAtIndex(bi)
      val ap = aw >>>> (56 - ars)
      val bp = bw >>>> (104 - brs)

      val v = ap.toShort |||| bp.toShort
      v
    }

    final def int(i: Long): Int = {
      val ai = i / elemBitSize
      val bi = ai + 1
      val o = i % elemBitSize
      val ars = o + 8
      val brs = (o - 8) max 0
      val aw = getAtIndex(ai)
      val bw = getAtIndex(bi)
      val ap = aw >>>> (40 - ars)
      val bp = bw >>>> (88 - brs)

      val v = ap.toInt |||| bp.toInt
      v
    }

    final def long(i: Long): Long = {
      val b = i / 64
      val o = i % 64
      val n = 64 - o

      getAtIndex(b + 0) <<<< o |||| getAtIndex(b + 1) >>>> n
    }

    final def update(i: Long, size: Long, v: Long): Unit = {
      @tailrec
      def go(i: Long, size: Long, v: Long): Unit = {
        val b = i / elemBitSize
        val o = i % elemBitSize
        val p = elemBitSize - o

        // offset:  |----------- o ----------| |------------------ p ---------------------|   |----------- o ----------| |------------------ p ---------------------|
        // offset:                    |-ezis-| |-------------------------- size ------------------------------| |-ezis-|
        // data:                      vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv   vvvvvvvv vvvvvvvV
        // data:                               Yyyyyyyy yyyyyyyy yyyyyyyy yyyyyyyy yyyyyyyy   yyyyyyyy yyyyyyyy
        // data:    Tttttttt tttttttt tttttttt uuuuuuuu uuuuuuuu uuuuuuuu uuuuuuuu uuuuuuuU   Wwwwwwww wwwwwwww xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxX
        // data:    Eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeeE   Ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff fffffffF

        // offset:           |----------- o ----------| |------------------ p ---------------------|
        // offset:  |---------------ezis--------------| |------------ size ---------------|
        // data:    vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvV
        // data:                                        Yyyyyyyy yyyyyyyy yyyyyyyy yyyyyyyy
        // data:             Tttttttt tttttttt tttttttt uuuuuuuu uuuuuuuu uuuuuuuu uuuuuuuu wwwwwwwW
        // data:             Eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeeE

        val ezis = elemBitSize - size
        val e = getAtIndex(b + 0)
        val y = v <<<< ezis
        val t = e >>>> p <<<< p
        val u = y >>>> o
        val w = e <<<< (o + size) >>>> (o + size)
        val ee = t |||| u |||| w

        setAtIndex(b + 0, ee)

        if (o + size > elemBitSize) {
          go(i + p, size - p, v)
        } else {
          ()
        }
      }

      go(i, size, v)
    }

    def signed(i: Long, size: Long): Long = {
      @tailrec
      def go(i: Long, size: Long, acc: Long): Long = {
        val b = i / elemBitSize
        val o = i % elemBitSize
        val s = ((size + o) min elemBitSize) - o
        val p = elemBitSize - o

        val e = getAtIndex(b + 0)
        val v = acc <<<< s |||| (e <<<< o >>>> o >>>> (p - s))

        if (size > s) {
          go(i + p, size - s, v)
        } else {
          v
        }
      }

      go(i, size, -1L)
    }
  }

  implicit class IntBitArrayOps_2s8EdpV(val array: Array[Int]) extends AnyVal {
    @inline final def elemBitSize: Int = 32

    final def setAtIndex(i: Long, v: Int): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Int = array(i.toInt)

    final def byte(i: Long, v: Byte): Unit = update(i, 8, v.ulong)
    final def short(i: Long, v: Short): Unit = update(i, 16, v.ulong)
    final def int(i: Long, v: Int): Unit = update(i, 32, v.ulong)
    final def long(i: Long, v: Long): Unit = update(i, 64, v.ulong)

    final def byte(i: Long): Byte = {
      val ai = i / elemBitSize
      val bi = ai + 1
      val o = i % elemBitSize
      val ars = o + 8
      val brs = (o - 8) max 0
      val aw = getAtIndex(ai)
      val bw = getAtIndex(bi)
      val ap = aw >>>> (32 - ars)
      val bp = bw >>>> (48 - brs)

      ap.toByte |||| bp.toByte
    }

    final def short(i: Long): Short = {
      val ai = i / elemBitSize
      val bi = ai + 1
      val o = i % elemBitSize
      val ars = o + 8
      val brs = (o - 8) max 0
      val aw = getAtIndex(ai)
      val bw = getAtIndex(bi)
      val ap = aw >>>> (24 - ars)
      val bp = bw >>>> (40 - brs)
      val v = ap.toShort |||| bp.toShort
      v
    }

    final def int(i: Long): Int = {
      val b = i / 32
      val o = i % 32
      val n = o - 32

      getAtIndex(b + 0) <<<< o |||| getAtIndex(b + 1) <<<< n
    }

    final def long(i: Long): Long = {
      val b = i / 32
      val o = i % 32
      val n = o - 32
      val p = o + 32

      val pp = getAtIndex(b + 0).ulong <<<< p
      val oo = getAtIndex(b + 1).ulong <<<< o
      val nn = getAtIndex(b + 2).ulong <<<< n

      pp |||| oo |||| nn
    }

    final def update(i: Long, size: Long, v: Long): Unit = {
      @tailrec
      def go(i: Long, size: Long, v: Long): Unit = {
        val b = i / elemBitSize
        val o = i % elemBitSize
        val p = elemBitSize - o

        // offset:  |----------- o ----------| |------------------ p ---------------------|   |----------- o ----------| |------------------ p ---------------------|
        // offset:                    |-ezis-| |-------------------------- size ------------------------------| |-ezis-|
        // data:                      vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv   vvvvvvvv vvvvvvvV
        // data:                               Yyyyyyyy yyyyyyyy yyyyyyyy yyyyyyyy yyyyyyyy   yyyyyyyy yyyyyyyy
        // data:    Tttttttt tttttttt tttttttt uuuuuuuu uuuuuuuu uuuuuuuu uuuuuuuu uuuuuuuU   Wwwwwwww wwwwwwww xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxX
        // data:    Eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeeE   Ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff fffffffF

        // offset:           |----------- o ----------| |------------------ p ---------------------|
        // offset:  |---------------ezis--------------| |------------ size ---------------|
        // data:    vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvV
        // data:                                        Yyyyyyyy yyyyyyyy yyyyyyyy yyyyyyyy
        // data:             Tttttttt tttttttt tttttttt uuuuuuuu uuuuuuuu uuuuuuuu uuuuuuuu wwwwwwwW
        // data:             Eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeeE

        val ezis = elemBitSize - size
        val e = getAtIndex(b + 0)
        val y = v <<<< ezis
        val t = e >>>> p <<<< p
        val u = (y >>>> o).uint
        val w = e <<<< (o + size) >>>> (o + size)
        val ee = t |||| u |||| w

        setAtIndex(b + 0, ee)

        if (o + size > elemBitSize) {
          go(i + p, size - p, v)
        } else {
          ()
        }
      }

      go(i, size, v)
    }
  }

  implicit class ShortBitArrayOps_2s8EdpV(val array: Array[Short]) extends AnyVal {
    @inline final def elemBitSize: Int = 16

    final def setAtIndex(i: Long, v: Short): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Short = array(i.toInt)

    final def byte(i: Long, v: Byte): Unit = update(i, 8, v.ulong)
    final def short(i: Long, v: Short): Unit = update(i, 16, v.ulong)
    final def int(i: Long, v: Int): Unit = update(i, 32, v.ulong)
    final def long(i: Long, v: Long): Unit = update(i, 64, v.ulong)

    final def byte(i: Long): Byte = {
      val ai = i / 16
      val bi = ai + 1
      val o = i % 16
      val ars = o + 8
      val brs = (o - 8) max 0
      val aw = getAtIndex(ai)
      val bw = getAtIndex(bi)
      val ap = aw >>>> (16 - ars)
      val bp = bw >>>> (16 - brs)

      ap.toByte |||| bp.toByte
    }

    final def short(i: Long): Short = {
      val b = i / 16
      val o = i % 16
      val n = o - 16

      val oo = getAtIndex(b + 0) <<<< o
      val nn = getAtIndex(b + 1) <<<< n

      oo |||| nn
    }

    final def int(i: Long): Int = {
      val b = i / 16
      val o = i % 16
      val n = o - 16
      val p = o + 16

      val pp = getAtIndex(b + 0).uint <<<< p
      val oo = getAtIndex(b + 1).uint <<<< o
      val nn = getAtIndex(b + 2).uint <<<< n

      pp |||| oo |||| nn
    }

    final def long(i: Long): Long = {
      val b = i / 16
      val o = i % 16
      val n = o - 16
      val p = o + 16
      val q = o + 32
      val r = o + 48

      val rr = getAtIndex(b + 0).ulong <<<< r
      val qq = getAtIndex(b + 1).ulong <<<< q
      val pp = getAtIndex(b + 2).ulong <<<< p
      val oo = getAtIndex(b + 3).ulong <<<< o
      val nn = getAtIndex(b + 4).ulong <<<< n

      rr |||| qq |||| pp |||| oo |||| nn
    }

    final def update(i: Long, size: Long, v: Long): Unit = {
      @tailrec
      def go(i: Long, size: Long, v: Long): Unit = {
        val b = i / elemBitSize
        val o = i % elemBitSize
        val p = elemBitSize - o

        // offset:  |----------- o ----------| |------------------ p ---------------------|   |----------- o ----------| |------------------ p ---------------------|
        // offset:                    |-ezis-| |-------------------------- size ------------------------------| |-ezis-|
        // data:                      vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv   vvvvvvvv vvvvvvvV
        // data:                               Yyyyyyyy yyyyyyyy yyyyyyyy yyyyyyyy yyyyyyyy   yyyyyyyy yyyyyyyy
        // data:    Tttttttt tttttttt tttttttt uuuuuuuu uuuuuuuu uuuuuuuu uuuuuuuu uuuuuuuU   Wwwwwwww wwwwwwww xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxX
        // data:    Eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeeE   Ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff fffffffF

        // offset:           |----------- o ----------| |------------------ p ---------------------|
        // offset:  |---------------ezis--------------| |------------ size ---------------|
        // data:    vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvV
        // data:                                        Yyyyyyyy yyyyyyyy yyyyyyyy yyyyyyyy
        // data:             Tttttttt tttttttt tttttttt uuuuuuuu uuuuuuuu uuuuuuuu uuuuuuuu wwwwwwwW
        // data:             Eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeeE

        val ezis = elemBitSize - size
        val e = getAtIndex(b + 0)
        val y = v <<<< ezis
        val t = e >>>> p <<<< p
        val u = (y >>>> o).ushort
        val w = e <<<< (o + size) >>>> (o + size)
        val ee = t |||| u |||| w

        setAtIndex(b + 0, ee)

        if (o + size > elemBitSize) {
          go(i + p, size - p, v)
        } else {
          ()
        }
      }

      go(i, size, v)
    }
  }

  implicit class ByteBitArrayOps_2s8EdpV(val array: Array[Byte]) extends AnyVal {
    @inline final def elemBitSize: Int = 8

    final def setAtIndex(i: Long, v: Byte): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Byte = array(i.toInt)

    final def byte(i: Long, v: Byte): Unit = update(i, 8, v.ulong)
    final def short(i: Long, v: Short): Unit = update(i, 16, v.ulong)
    final def int(i: Long, v: Int): Unit = update(i, 32, v.ulong)
    final def long(i: Long, v: Long): Unit = update(i, 64, v.ulong)

    final def byte(i: Long): Byte = {
      val b = i / 8
      val o = i % 8
      val n = o - 8

      getAtIndex(b + 0) <<<< o |||| getAtIndex(b + 1) <<<< n
    }

    final def short(i: Long): Short = {
      val b = i / 8
      val o = i % 8
      val n = o - 8
      val p = o + 8

      val pp = getAtIndex(b + 0).ushort <<<< p
      val oo = getAtIndex(b + 1).ushort <<<< o
      val nn = getAtIndex(b + 2).ushort <<<< n

      pp |||| oo |||| nn
    }

    final def int(i: Long): Int = {
      val b = i / 8
      val o = i % 8
      val n = o - 8
      val p = o + 8
      val q = o + 16
      val r = o + 24

      val rr = getAtIndex(b + 0).uint <<<< r
      val qq = getAtIndex(b + 1).uint <<<< q
      val pp = getAtIndex(b + 2).uint <<<< p
      val oo = getAtIndex(b + 3).uint <<<< o
      val nn = getAtIndex(b + 4).uint <<<< n

      rr |||| qq |||| pp |||| oo |||| nn
    }

    final def long(i: Long): Long = {
      val b = i / 8
      val o = i % 8
      val n = o - 8
      val p = o + 8
      val q = o + 16
      val r = o + 24
      val s = o + 32
      val t = o + 40
      val u = o + 48
      val v = o + 56

      val vv = getAtIndex(b + 0).ulong <<<< v
      val uu = getAtIndex(b + 1).ulong <<<< u
      val tt = getAtIndex(b + 2).ulong <<<< t
      val ss = getAtIndex(b + 3).ulong <<<< s
      val rr = getAtIndex(b + 4).ulong <<<< r
      val qq = getAtIndex(b + 5).ulong <<<< q
      val pp = getAtIndex(b + 6).ulong <<<< p
      val oo = getAtIndex(b + 7).ulong <<<< o
      val nn = getAtIndex(b + 8).ulong <<<< n

      vv |||| uu |||| tt |||| ss |||| rr |||| qq |||| pp |||| oo |||| nn
    }

    final def update(i: Long, size: Long, v: Long): Unit = {
      @tailrec
      def go(i: Long, size: Long, v: Long): Unit = {
        val b = i / elemBitSize
        val o = i % elemBitSize
        val p = elemBitSize - o

        // offset:  |----------- o ----------| |------------------ p ---------------------|   |----------- o ----------| |------------------ p ---------------------|
        // offset:                    |-ezis-| |-------------------------- size ------------------------------| |-ezis-|
        // data:                      vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv   vvvvvvvv vvvvvvvV
        // data:                               Yyyyyyyy yyyyyyyy yyyyyyyy yyyyyyyy yyyyyyyy   yyyyyyyy yyyyyyyy
        // data:    Tttttttt tttttttt tttttttt uuuuuuuu uuuuuuuu uuuuuuuu uuuuuuuu uuuuuuuU   Wwwwwwww wwwwwwww xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxX
        // data:    Eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeeE   Ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff fffffffF

        // offset:           |----------- o ----------| |------------------ p ---------------------|
        // offset:  |---------------ezis--------------| |------------ size ---------------|
        // data:    vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvv vvvvvvvV
        // data:                                        Yyyyyyyy yyyyyyyy yyyyyyyy yyyyyyyy
        // data:             Tttttttt tttttttt tttttttt uuuuuuuu uuuuuuuu uuuuuuuu uuuuuuuu wwwwwwwW
        // data:             Eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeee eeeeeeeE

        val ezis = elemBitSize - size
        val e = getAtIndex(b + 0)
        val y = v <<<< ezis
        val t = e >>>> p <<<< p
        val u = (y >>>> o).ubyte
        val w = e <<<< (o + size) >>>> (o + size)
        val ee = t |||| u |||| w

        setAtIndex(b + 0, ee)

        if (o + size > elemBitSize) {
          go(i + p, size - p, v)
        } else {
          ()
        }
      }

      go(i, size, v)
    }
  }
}
