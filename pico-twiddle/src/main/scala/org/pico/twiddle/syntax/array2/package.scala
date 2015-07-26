package org.pico.twiddle.syntax

import org.pico.twiddle.{FixedInt2FixedInt, FixedInt}
import org.pico.twiddle.instances._
import org.pico.twiddle.syntax.fixedInt._
import org.pico.twiddle.syntax.fixedInt2FixedInt._

import scala.annotation.tailrec

package object array2 {
  implicit class ArrayOps_2s8EdpV[A](val self: Array[A]) extends AnyVal {
    @inline final def setAtIndex(i: Long, v: Long)(implicit ev: FixedInt[A], ev2: FixedInt2FixedInt[Long, A]): Unit = self(i.toInt) = v.fixAs[A]

    @inline final def getAtIndex(i: Long)(implicit ev: FixedInt[A], ev2: FixedInt2FixedInt[Long, A]): Long = self(i.uint).ulong

    @inline final def byte(i: Long, v: Byte)(implicit ev: FixedInt[A], ev2: FixedInt2FixedInt[Long, A]): Unit = update(i, 8, v.ulong)
    @inline final def short(i: Long, v: Short)(implicit ev: FixedInt[A], ev2: FixedInt2FixedInt[Long, A]): Unit = update(i, 16, v.ulong)
    @inline final def int(i: Long, v: Int)(implicit ev: FixedInt[A], ev2: FixedInt2FixedInt[Long, A]): Unit = update(i, 32, v.ulong)
    @inline final def long(i: Long, v: Long)(implicit ev: FixedInt[A], ev2: FixedInt2FixedInt[Long, A]): Unit = update(i, 64, v.ulong)

    @inline final def byte(i: Long)(implicit ev: FixedInt[A], ev2: FixedInt2FixedInt[Long, A]): Byte = signed(i, 8).ubyte
    @inline final def short(i: Long)(implicit ev: FixedInt[A], ev2: FixedInt2FixedInt[Long, A]): Short = signed(i, 16).ushort
    @inline final def int(i: Long)(implicit ev: FixedInt[A], ev2: FixedInt2FixedInt[Long, A]): Int = signed(i, 32).uint
    @inline final def long(i: Long)(implicit ev: FixedInt[A], ev2: FixedInt2FixedInt[Long, A]): Long = signed(i, 64).ulong

    @inline final def update(i: Long, size: Long, v: Long)(implicit ev: FixedInt[A], ev2: FixedInt2FixedInt[Long, A]): Unit = {
      @tailrec
      def go(i: Long, size: Long, v: Long): Unit = {
        val b = i / bitSize[A]
        val o = i % bitSize[A]
        val p = bitSize[A] - o

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

        val ezis = bitSize[A] - size
        val e = getAtIndex(b)
        val y = v <<<< ezis
        val t = e >>>> p <<<< p
        val u = y >>>> o
        val w = e <<<< (o + size) >>>> (o + size)
        val ee = t |||| u |||| w

        setAtIndex(b, ee)

        if (o + size > bitSize[A]) {
          go(i + p, size - p, v)
        } else {
          ()
        }
      }

      go(i, size, v)
    }

    @inline def signed(i: Long, size: Long)(implicit ev: FixedInt[A], ev2: FixedInt2FixedInt[Long, A]): Long = {
      @tailrec
      def go(i: Long, size: Long, acc: Long): Long = {
        val b = i / bitSize[A]
        val o = i % bitSize[A]
        val s = ((size + o) min bitSize[A]) - o
        val p = bitSize[A] - o

        val e = getAtIndex(b).ulong
        val l = acc <<<< s
        val r = e <<<< o >>>> bitSize[A] - s
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
