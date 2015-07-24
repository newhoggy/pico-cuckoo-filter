package org.pico.twiddle.array

import org.pico.twiddle.unsigned.syntax._
import org.pico.twiddle.syntax.anyVal._

package object syntax {
  implicit class LongBitArrayOps_2s8EdpV(val array: Array[Long]) extends AnyVal {
    @inline final def elemBitSize: Int = 64

    final def setAtIndex(i: Long, v: Long): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Long = array(i.toInt)

    final def byte(i: Long, v: Byte): Unit = {
      val ai = i / elemBitSize
      val bi = ai + 1
      val o = i % elemBitSize
      val ars = o + v.bitSize
      val als = elemBitSize + v.bitSize - ars
      val brs = (o - elemBitSize + v.bitSize) max 0
      val bls = elemBitSize + v.bitSize - brs
      val aw = getAtIndex(ai)
      val bw = getAtIndex(bi)
      val ax = aw <<<< ars >>>> ars |||| aw >>>> als <<<< als
      val bx = bw <<<< brs >>>> brs |||| bw >>>> bls <<<< bls
      val av = v.ulong >>>> (o + v.bitSize - elemBitSize * 1)
      val bv = v.ulong >>>> (o + v.bitSize - elemBitSize * 2)

      setAtIndex(ai, ax |||| av)
      setAtIndex(bi, bx |||| bv)
    }

    final def byte(i: Long): Byte = {
      val ai = i / elemBitSize
      val bi = ai + 1
      val o = i % elemBitSize
      val ars = o + 8
      val brs = (o - 8) max 0
      val aw = getAtIndex(ai)
      val bw = getAtIndex(bi)
      val ap = aw >>>> (64 - ars)
      val bp = bw >>>> (112 - brs)
      val v = ap.toByte |||| bp.toByte
      v
    }

    final def short(i: Long, v: Short): Unit = {
      val ai = i / elemBitSize
      val bi = ai + 1
      val o = i % elemBitSize
      val ars = o + v.bitSize
      val als = elemBitSize + v.bitSize - ars
      val brs = (o - elemBitSize + v.bitSize) max 0
      val bls = elemBitSize + v.bitSize - brs
      val aw = getAtIndex(ai)
      val bw = getAtIndex(bi)
      val ax = aw <<<< ars >>>> ars |||| aw >>>> als <<<< als
      val bx = bw <<<< brs >>>> brs |||| bw >>>> bls <<<< bls
      val av = v.ulong >>>> (o + v.bitSize - elemBitSize * 1)
      val bv = v.ulong >>>> (o + v.bitSize - elemBitSize * 2)

      setAtIndex(ai, ax |||| av)
      setAtIndex(bi, bx |||| bv)
    }

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

    final def int(i: Long, v: Int): Unit = {
      val b = i / 64
      val o = i % 64
      val n = 64 - o

      setAtIndex(b + 0, getAtIndex(b + 0) >>>> n <<<< n |||| v.ulong >>>> o)
      setAtIndex(b + 1, getAtIndex(b + 1) <<<< o >>>> o |||| v.ulong <<<< n)
    }

    final def int(i: Long): Int = {
      val b = i / 64
      val o = i % 64
      val n = 64 - o

      (getAtIndex(b + 0) <<<< o |||| getAtIndex(b + 1) >>>> n).toInt
    }

    final def long(i: Long, v: Long): Unit = {
      val b = i / 64
      val o = i % 64
      val n = 64 - o

      setAtIndex(b + 0, getAtIndex(b + 0) >>>> n <<<< n |||| v >>>> o)
      setAtIndex(b + 1, getAtIndex(b + 1) <<<< o >>>> o |||| v <<<< n)
    }

    final def long(i: Long): Long = {
      val b = i / 64
      val o = i % 64
      val n = 64 - o

      getAtIndex(b + 0) <<<< o |||| getAtIndex(b + 1) >>>> n
    }
  }

  implicit class IntBitArrayOps_2s8EdpV(val array: Array[Int]) extends AnyVal {
    @inline final def elemBitSize: Int = 32

    final def setAtIndex(i: Long, v: Int): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Int = array(i.toInt)

    final def byte(i: Long, v: Byte): Unit = {
      val ai = i / elemBitSize
      val bi = ai + 1
      val o = i % elemBitSize
      val ars = o + v.bitSize
      val als = elemBitSize + v.bitSize - ars
      val brs = (o - elemBitSize + v.bitSize) max 0
      val bls = elemBitSize + v.bitSize - brs
      val aw = getAtIndex(ai)
      val bw = getAtIndex(bi)
      val ax = aw <<<< ars >>>> ars |||| aw >>>> als <<<< als
      val bx = bw <<<< brs >>>> brs |||| bw >>>> bls <<<< bls
      val av = v.uint >>>> (o + v.bitSize - elemBitSize * 1)
      val bv = v.uint >>>> (o + v.bitSize - elemBitSize * 2)

      setAtIndex(ai, ax |||| av)
      setAtIndex(bi, bx |||| bv)
    }

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

    final def short(i: Long, v: Short): Unit = {
      val ai = i / elemBitSize
      val bi = ai + 1
      val o = i % elemBitSize
      val ars = o + v.bitSize
      val als = elemBitSize + v.bitSize - ars
      val brs = (o - elemBitSize + v.bitSize) max 0
      val bls = elemBitSize + v.bitSize - brs
      val aw = getAtIndex(ai)
      val bw = getAtIndex(bi)
      val ax = aw <<<< ars >>>> ars |||| aw >>>> als <<<< als
      val bx = bw <<<< brs >>>> brs |||| bw >>>> bls <<<< bls
      val av = v.uint >>>> (o + v.bitSize - elemBitSize * 1)
      val bv = v.uint >>>> (o + v.bitSize - elemBitSize * 2)

      setAtIndex(ai, ax |||| av)
      setAtIndex(bi, bx |||| bv)
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

    final def int(i: Long, v: Int): Unit = {
      val b = i / 32
      val o = i % 32
      val n = o - 32

      setAtIndex(b + 0, getAtIndex(b + 0) <<<< n >>>> n |||| v >>>> o)
      setAtIndex(b + 1, getAtIndex(b + 1) <<<< o >>>> o |||| v >>>> n)
    }

    final def int(i: Long): Int = {
      val b = i / 32
      val o = i % 32
      val n = o - 32

      getAtIndex(b + 0) <<<< o |||| getAtIndex(b + 1) <<<< n
    }

    final def long(i: Long, v: Long): Unit = {
      val b = i / 32
      val o = i % 32
      val n = o - 32
      val p = o + 32

      setAtIndex(b + 0, getAtIndex(b + 0) <<<< n >>>> n |||| (v >>>> p).toInt)
      setAtIndex(b + 1,                                      (v >>>> o).toInt)
      setAtIndex(b + 2, getAtIndex(b + 2) <<<< o >>>> o |||| (v >>>> n).toInt)
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
  }

  implicit class ShortBitArrayOps_2s8EdpV(val array: Array[Short]) extends AnyVal {
    final def setAtIndex(i: Long, v: Short): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Short = array(i.toInt)

    final def byte(i: Long, v: Byte): Unit = {
      val ai = i / 16
      val bi = ai + 1
      val o = i % 16
      val ars = o + 8
      val als = 16 + 8 - ars
      val brs = (o - 8) max 0
      val bls = 16 + 8 - brs

      val aw = getAtIndex(ai)
      val bw = getAtIndex(bi)

      val ax = aw <<<< ars >>>> ars |||| aw >>>> als <<<< als
      val bx = bw <<<< brs >>>> brs |||| bw >>>> bls <<<< bls

      val av = v.ushort >>>> (o - 8)
      val bv = v.ushort >>>> (o - 24)

      setAtIndex(ai, ax |||| av)
      setAtIndex(bi, bx |||| bv)
    }

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

    final def short(i: Long, v: Short): Unit = {
      val b = i / 16
      val o = i % 16
      val n = o - 16

      setAtIndex(b + 0, getAtIndex(b + 0) <<<< n >>>> n |||| v >>>> o)
      setAtIndex(b + 1, getAtIndex(b + 1) <<<< o >>>> o |||| v >>>> n)
    }

    final def short(i: Long): Short = {
      val b = i / 16
      val o = i % 16
      val n = o - 16

      val oo = getAtIndex(b + 0) <<<< o
      val nn = getAtIndex(b + 1) <<<< n

      oo |||| nn
    }

    final def int(i: Long, v: Int): Unit = {
      val b = i / 16
      val o = i % 16
      val n = o - 16
      val p = o + 16

      setAtIndex(b + 0, getAtIndex(b + 0) <<<< n >>>> n |||| (v >>>> p).toShort)
      setAtIndex(b + 1,                                      (v >>>> o).toShort)
      setAtIndex(b + 2, getAtIndex(b + 2) <<<< o >>>> o |||| (v >>>> n).toShort)
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

    final def long(i: Long, v: Long): Unit = {
      val b = i / 16
      val o = i % 16
      val n = o - 16
      val p = o + 16
      val q = o + 32
      val r = o + 48

      setAtIndex(b + 0, getAtIndex(b + 0) <<<< n >>>> n |||| (v >>>> r).toShort)
      setAtIndex(b + 1,                                      (v >>>> q).toShort)
      setAtIndex(b + 2,                                      (v >>>> p).toShort)
      setAtIndex(b + 3,                                      (v >>>> o).toShort)
      setAtIndex(b + 4, getAtIndex(b + 4) <<<< o >>>> o |||| (v >>>> n).toShort)
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
  }

  implicit class ByteBitArrayOps_2s8EdpV(val array: Array[Byte]) extends AnyVal {
    final def setAtIndex(i: Long, v: Byte): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Byte = array(i.toInt)

    final def byte(i: Long, v: Byte): Unit = {
      val b = i / 8
      val o = i % 8
      val n = o - 8

      setAtIndex(b + 0, getAtIndex(b + 0) <<<< n >>>> n |||| v >>>> o)
      setAtIndex(b + 1, getAtIndex(b + 1) <<<< o >>>> o |||| v >>>> n)
    }

    final def byte(i: Long): Byte = {
      val b = i / 8
      val o = i % 8
      val n = o - 8

      getAtIndex(b + 0) <<<< o |||| getAtIndex(b + 1) <<<< n
    }

    final def short(i: Long, v: Short): Unit = {
      val b = i / 8
      val o = i % 8
      val n = o - 8
      val p = o + 8

      val pp = getAtIndex(b + 0) <<<< n >>>> n |||| (v >>>> p).toByte
      val oo =                                      (v >>>> o).toByte
      val nn = getAtIndex(b + 2) <<<< o >>>> o |||| (v >>>> n).toByte

      setAtIndex(b + 0, pp)
      setAtIndex(b + 1, oo)
      setAtIndex(b + 2, nn)
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

    final def int(i: Long, v: Int): Unit = {
      val b = i / 8
      val o = i % 8
      val n = o - 8
      val p = o + 8
      val q = o + 16
      val r = o + 24

      setAtIndex(b + 0, getAtIndex(b + 0) <<<< n >>>> n |||| (v >>>> r).toByte)
      setAtIndex(b + 1,                                      (v >>>> q).toByte)
      setAtIndex(b + 2,                                      (v >>>> p).toByte)
      setAtIndex(b + 3,                                      (v >>>> o).toByte)
      setAtIndex(b + 4, getAtIndex(b + 4) <<<< o >>>> o |||| (v >>>> n).toByte)
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

    final def long(i: Long, value: Long): Unit = {
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

      setAtIndex(b + 0, getAtIndex(b + 0) <<<< n >>>> n |||| (value >>>> v).toByte)
      setAtIndex(b + 1,                                      (value >>>> u).toByte)
      setAtIndex(b + 2,                                      (value >>>> t).toByte)
      setAtIndex(b + 3,                                      (value >>>> s).toByte)
      setAtIndex(b + 4,                                      (value >>>> r).toByte)
      setAtIndex(b + 5,                                      (value >>>> q).toByte)
      setAtIndex(b + 6,                                      (value >>>> p).toByte)
      setAtIndex(b + 7,                                      (value >>>> o).toByte)
      setAtIndex(b + 8, getAtIndex(b + 8) <<<< o >>>> o |||| (value >>>> n).toByte)
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
  }
}
