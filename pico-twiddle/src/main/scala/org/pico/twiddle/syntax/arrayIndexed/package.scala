package org.pico.twiddle.syntax

import org.pico.twiddle._
import org.pico.twiddle.instances._
import org.pico.twiddle.syntax.fixedInt._
import org.pico.twiddle.syntax.fixedInt2FixedInt._

import scala.annotation.tailrec

import scala.language.higherKinds

package object arrayIndexed {
  implicit class ArrayIndexedOps[F[_], E](val self: F[E]) extends AnyVal {
    @inline final def bitsString(i: Bits, size: Bits): String = ???

    @inline final def setAtIndex(
        i: Bits, e: E)(implicit ev: ArrayIndexed[F, E]): Unit = ev.setAtIndex(self, i, e)

    @inline final def getAtIndex(
        i: Bits)(implicit ev: ArrayIndexed[F, E]): E = ev.getAtIndex(self, i)

    @inline final def byte(i: Bits, v: Byte)(
        implicit  ev0: FixedInt[E],
        ev1: FixedInt2FixedInt[Long, E],
        ev2: ArrayIndexed[F, E]): Unit = update(i, Bits(8), v.ulong)

    @inline final def short(i: Bits, v: Short)(
        implicit  ev0: FixedInt[E],
        ev1: FixedInt2FixedInt[Long, E],
        ev2: ArrayIndexed[F, E]): Unit = update(i, Bits(16), v.ulong)

    @inline final def int(i: Bits, v: Int)(
        implicit  ev0: FixedInt[E],
        ev1: FixedInt2FixedInt[Long, E],
        ev2: ArrayIndexed[F, E]): Unit = update(i, Bits(32), v.ulong)

    @inline final def long(i: Bits, v: Long)(
        implicit  ev0: FixedInt[E],
        ev1: FixedInt2FixedInt[Long, E],
        ev2: ArrayIndexed[F, E]): Unit = update(i, Bits(64), v.ulong)

    @inline final def byte(i: Bits)(
        implicit  ev0: FixedInt[E],
        ev1: FixedInt2FixedInt[Long, E],
        ev2: ArrayIndexed[F, E]): Byte = signed(i, Bits(8)).ubyte

    @inline final def short(i: Bits)(
        implicit  ev0: FixedInt[E],
        ev1: FixedInt2FixedInt[Long, E],
        ev2: ArrayIndexed[F, E]): Short = signed(i, Bits(16)).ushort

    @inline final def int(i: Bits)(
        implicit  ev0: FixedInt[E],
        ev1: FixedInt2FixedInt[Long, E],
        ev2: ArrayIndexed[F, E]): Int = signed(i, Bits(32)).uint

    @inline final def long(i: Bits)(
        implicit  ev0: FixedInt[E],
        ev1: FixedInt2FixedInt[Long, E],
        ev2: ArrayIndexed[F, E]): Long = signed(i, Bits(64)).ulong

    @inline final def update(i: Bits, size: Bits, v: Long)(
        implicit  ev0: FixedInt[E],
        ev1: FixedInt2FixedInt[Long, E],
        ev2: ArrayIndexed[F, E]): Unit = {
      @tailrec
      def go(i: Bits, size: Bits, v: Long): Unit = {
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

    @inline def signed(i: Bits, size: Bits)(
        implicit  ev0: FixedInt[E],
        ev1: FixedInt2FixedInt[Long, E],
        ev2: ArrayIndexed[F, E]): Long = {
      @tailrec
      def go(i: Bits, size: Bits, acc: Long): Long = {
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

    @inline def unsigned(i: Bits, size: Bits)(
        implicit  ev0: FixedInt[E],
        ev1: FixedInt2FixedInt[Long, E],
        ev2: ArrayIndexed[F, E]): Long = {
      val shift = bitSize[Long] - size

      signed(i, size) <<<< shift >>>> shift
    }
  }
}
