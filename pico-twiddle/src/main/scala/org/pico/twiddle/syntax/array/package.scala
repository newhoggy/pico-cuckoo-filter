package org.pico.twiddle.syntax

import org.pico.twiddle.Bits
import org.pico.twiddle.unsigned.syntax._

import scala.annotation.tailrec

package object array {
  implicit class LongBitArrayOps_2s8EdpV(val array: Array[Long]) extends AnyVal {
    @inline final def elemBitSize: Bits = Bits(64)

    @inline final def setAtIndex(i: Bits, v: Long): Unit = array(i.value.toInt) = v

    @inline final def getAtIndex(i: Bits): Long = array(i.value.toInt)

    @inline final def byte(i: Bits, v: Byte): Unit = update(i, Bits(8), v.ulong)
    @inline final def short(i: Bits, v: Short): Unit = update(i, Bits(16), v.ulong)
    @inline final def int(i: Bits, v: Int): Unit = update(i, Bits(32), v.ulong)
    @inline final def long(i: Bits, v: Long): Unit = update(i, Bits(64), v.ulong)

    @inline final def byte(i: Bits): Byte = signed(i, Bits(8)).ubyte
    @inline final def short(i: Bits): Short = signed(i, Bits(16)).ushort
    @inline final def int(i: Bits): Int = signed(i, Bits(32)).uint
    @inline final def long(i: Bits): Long = signed(i, Bits(64)).ulong

    @inline final def update(i: Bits, size: Bits, v: Long): Unit = {
      @tailrec
      def go(i: Bits, size: Bits, v: Long): Unit = {
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
        val e = getAtIndex(b)
        val y = v <<<< ezis
        val t = e >>>> p <<<< p
        val u = y >>>> o
        val w = e <<<< (o + size) >>>> (o + size)
        val ee = t |||| u |||| w

        setAtIndex(b, ee)

        if (o + size > elemBitSize) {
          go(i + p, size - p, v)
        } else {
          ()
        }
      }

      go(i, size, v)
    }

    @inline def signed(i: Bits, size: Bits): Long = {
      @tailrec
      def go(i: Bits, size: Bits, acc: Long): Long = {
        val b = i / elemBitSize
        val o = i % elemBitSize
        val s = ((size + o) min elemBitSize) - o
        val p = elemBitSize - o

        val e = getAtIndex(b).ulong
        val l = acc <<<< s
        val r = e <<<< o >>>> elemBitSize - s
        val v = l |||| r

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
    @inline final def elemBitSize: Bits = Bits(32)

    @inline final def setAtIndex(i: Bits, v: Int): Unit = array(i.value.toInt) = v

    @inline final def getAtIndex(i: Bits): Int = array(i.value.toInt)

    @inline final def byte(i: Bits, v: Byte): Unit = update(i, Bits(8), v.ulong)
    @inline final def short(i: Bits, v: Short): Unit = update(i, Bits(16), v.ulong)
    @inline final def int(i: Bits, v: Int): Unit = update(i, Bits(32), v.ulong)
    @inline final def long(i: Bits, v: Long): Unit = update(i, Bits(64), v.ulong)

    @inline final def byte(i: Bits): Byte = signed(i, Bits(8)).ubyte
    @inline final def short(i: Bits): Short = signed(i, Bits(16)).ushort
    @inline final def int(i: Bits): Int = signed(i, Bits(32)).uint
    @inline final def long(i: Bits): Long = signed(i, Bits(64)).ulong

    @inline final def update(i: Bits, size: Bits, v: Long): Unit = {
      @tailrec
      def go(i: Bits, size: Bits, v: Long): Unit = {
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
        val e = getAtIndex(b)
        val y = v <<<< ezis
        val t = e >>>> p <<<< p
        val u = (y >>>> o).uint
        val w = e <<<< (o + size) >>>> (o + size)
        val ee = t |||| u |||| w

        setAtIndex(b, ee)

        if (o + size > elemBitSize) {
          go(i + p, size - p, v)
        } else {
          ()
        }
      }

      go(i, size, v)
    }

    @inline def signed(i: Bits, size: Bits): Long = {
      @tailrec
      def go(i: Bits, size: Bits, acc: Long): Long = {
        val b = i / elemBitSize
        val o = i % elemBitSize
        val s = ((size + o) min elemBitSize) - o
        val p = elemBitSize - o

        val e = getAtIndex(b).ulong
        val l = acc <<<< s
        val r = e <<<< o >>>> elemBitSize - s
        val v = l |||| r

        if (size > s) {
          go(i + p, size - s, v)
        } else {
          v
        }
      }

      go(i, size, -1L)
    }
  }

  implicit class ShortBitArrayOps_2s8EdpV(val array: Array[Short]) extends AnyVal {
    @inline final def elemBitSize: Bits = Bits(16)

    @inline final def setAtIndex(i: Bits, v: Short): Unit = array(i.value.toInt) = v

    @inline final def getAtIndex(i: Bits): Short = array(i.value.toInt)

    @inline final def byte(i: Bits, v: Byte): Unit = update(i, Bits(8), v.ulong)
    @inline final def short(i: Bits, v: Short): Unit = update(i, Bits(16), v.ulong)
    @inline final def int(i: Bits, v: Int): Unit = update(i, Bits(32), v.ulong)
    @inline final def long(i: Bits, v: Long): Unit = update(i, Bits(64), v.ulong)

    @inline final def byte(i: Bits): Byte = signed(i, Bits(8)).ubyte
    @inline final def short(i: Bits): Short = signed(i, Bits(16)).ushort
    @inline final def int(i: Bits): Int = signed(i, Bits(32)).uint
    @inline final def long(i: Bits): Long = signed(i, Bits(64)).ulong

    @inline final def update(i: Bits, size: Bits, v: Long): Unit = {
      @tailrec
      def go(i: Bits, size: Bits, v: Long): Unit = {
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
        val e = getAtIndex(b)
        val y = v <<<< ezis
        val t = e >>>> p <<<< p
        val u = (y >>>> o).ushort
        val w = e <<<< (o + size) >>>> (o + size)
        val ee = t |||| u |||| w

        setAtIndex(b, ee)

        if (o + size > elemBitSize) {
          go(i + p, size - p, v)
        } else {
          ()
        }
      }

      go(i, size, v)
    }

    @inline def signed(i: Bits, size: Bits): Long = {
      @tailrec
      def go(i: Bits, size: Bits, acc: Long): Long = {
        val b = i / elemBitSize
        val o = i % elemBitSize
        val s = ((size + o) min elemBitSize) - o
        val p = elemBitSize - o

        val e = getAtIndex(b).ulong
        val l = acc <<<< s
        val r = e <<<< o >>>> (elemBitSize - s)
        val v = l |||| r

        if (size > s) {
          go(i + p, size - s, v)
        } else {
          v
        }
      }

      go(i, size, -1L)
    }
  }

  implicit class ByteBitArrayOps_2s8EdpV(val array: Array[Byte]) extends AnyVal {
    @inline final def elemBitSize: Bits = Bits(8)

    @inline final def setAtIndex(i: Bits, v: Byte): Unit = array(i.value.toInt) = v

    @inline final def getAtIndex(i: Bits): Byte = array(i.value.toInt)

    @inline final def byte(i: Bits, v: Byte): Unit = update(i, Bits(8), v.ulong)
    @inline final def short(i: Bits, v: Short): Unit = update(i, Bits(16), v.ulong)
    @inline final def int(i: Bits, v: Int): Unit = update(i, Bits(32), v.ulong)
    @inline final def long(i: Bits, v: Long): Unit = update(i, Bits(64), v.ulong)

    @inline final def byte(i: Bits): Byte = signed(i, Bits(8)).ubyte
    @inline final def short(i: Bits): Short = signed(i, Bits(16)).ushort
    @inline final def int(i: Bits): Int = signed(i, Bits(32)).uint
    @inline final def long(i: Bits): Long = signed(i, Bits(64)).ulong

    @inline final def update(i: Bits, size: Bits, v: Long): Unit = {
      @tailrec
      def go(i: Bits, size: Bits, v: Long): Unit = {
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
        val e = getAtIndex(b)
        val y = v <<<< ezis
        val t = e >>>> p <<<< p
        val u = (y >>>> o).ubyte
        val w = e <<<< (o + size) >>>> (o + size)
        val ee = t |||| u |||| w

        setAtIndex(b, ee)

        if (o + size > elemBitSize) {
          go(i + p, size - p, v)
        } else {
          ()
        }
      }

      go(i, size, v)
    }

    @inline def signed(i: Bits, size: Bits): Long = {
      @tailrec
      def go(i: Bits, size: Bits, acc: Long): Long = {
        val b = i / elemBitSize
        val o = i % elemBitSize
        val s = ((size + o) min elemBitSize) - o
        val p = elemBitSize - o

        val e = getAtIndex(b).ulong
        val l = acc <<<< s
        val r = e <<<< o >>>> (elemBitSize - s)
        val v = l |||| r

        if (size > s) {
          go(i + p, size - s, v)
        } else {
          v
        }
      }

      go(i, size, -1L)
    }
  }
}
