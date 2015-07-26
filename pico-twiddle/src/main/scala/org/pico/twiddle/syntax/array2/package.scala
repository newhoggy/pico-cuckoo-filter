package org.pico.twiddle.syntax

import org.pico.twiddle.{ArrayIndexed, FixedInt2FixedInt, FixedInt}
import org.pico.twiddle.instances._
import org.pico.twiddle.syntax.fixedInt._
import org.pico.twiddle.syntax.fixedInt2FixedInt._

import scala.annotation.tailrec

package object array2 {
  implicit class ArrayOps_2s8EdpV[F[_], E](val self: F[E]) extends AnyVal {
    @inline final def setAtIndex(i: Long, v: E)(
        implicit  ev0: FixedInt[E],
                  ev1: FixedInt2FixedInt[Long, E],
                  ev2: ArrayIndexed[F, E]): Unit = ev2.setAtIndex(self, i.toInt, v)

    @inline final def getAtIndex(i: Long)(
        implicit  ev0: FixedInt[E],
                  ev1: FixedInt2FixedInt[Long, E],
                  ev2: ArrayIndexed[F, E]): E = ev2.getAtIndex(self, i.uint)

    @inline final def byte(i: Long, v: Byte)(
        implicit  ev0: FixedInt[E],
                  ev1: FixedInt2FixedInt[Long, E],
                  ev2: ArrayIndexed[F, E]): Unit = update(i, 8, v.ulong)

    @inline final def short(i: Long, v: Short)(
        implicit  ev0: FixedInt[E],
                  ev1: FixedInt2FixedInt[Long, E],
                  ev2: ArrayIndexed[F, E]): Unit = update(i, 16, v.ulong)

    @inline final def int(i: Long, v: Int)(
        implicit  ev0: FixedInt[E],
                  ev1: FixedInt2FixedInt[Long, E],
                  ev2: ArrayIndexed[F, E]): Unit = update(i, 32, v.ulong)

    @inline final def long(i: Long, v: Long)(
        implicit  ev0: FixedInt[E],
                  ev1: FixedInt2FixedInt[Long, E],
                  ev2: ArrayIndexed[F, E]): Unit = update(i, 64, v.ulong)

    @inline final def byte(i: Long)(
        implicit  ev0: FixedInt[E],
                  ev1: FixedInt2FixedInt[Long, E],
                  ev2: ArrayIndexed[F, E]): Byte = signed(i, 8).ubyte

    @inline final def short(i: Long)(
        implicit  ev0: FixedInt[E],
                  ev1: FixedInt2FixedInt[Long, E],
                  ev2: ArrayIndexed[F, E]): Short = signed(i, 16).ushort

    @inline final def int(i: Long)(
        implicit  ev0: FixedInt[E],
                  ev1: FixedInt2FixedInt[Long, E],
                  ev2: ArrayIndexed[F, E]): Int = signed(i, 32).uint

    @inline final def long(i: Long)(
        implicit  ev0: FixedInt[E],
                  ev1: FixedInt2FixedInt[Long, E],
                  ev2: ArrayIndexed[F, E]): Long = signed(i, 64).ulong

    @inline final def update(i: Long, size: Long, v: Long)(
        implicit  ev0: FixedInt[E],
                  ev1: FixedInt2FixedInt[Long, E],
                  ev2: ArrayIndexed[F, E]): Unit = {
      @tailrec
      def go(i: Long, size: Long, v: Long): Unit = {
        val b = i / bitSize[E]
        val o = i % bitSize[E]
        val p = bitSize[E] - o

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

        val ezis = bitSize[E] - size
        val e = getAtIndex(b)
        val y = (v <<<< ezis).fixAs[E]
        val t = e >>>> p <<<< p
        val u = y >>>> o
        val w = e <<<< (o + size) >>>> (o + size)
        val ee = t |||| u |||| w

        setAtIndex(b, ee)

        if (o + size > bitSize[E]) {
          go(i + p, size - p, v)
        } else {
          ()
        }
      }

      go(i, size, v)
    }

    @inline def signed(i: Long, size: Long)(
        implicit  ev0: FixedInt[E],
                  ev1: FixedInt2FixedInt[Long, E],
                  ev2: ArrayIndexed[F, E]): Long = {
      @tailrec
      def go(i: Long, size: Long, acc: Long): Long = {
        val b = i / bitSize[E]
        val o = i % bitSize[E]
        val s = ((size + o) min bitSize[E]) - o
        val p = bitSize[E] - o

        val e = getAtIndex(b).ulong
        val l = acc <<<< s
        val r = e <<<< o >>>> bitSize[E] - s
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
