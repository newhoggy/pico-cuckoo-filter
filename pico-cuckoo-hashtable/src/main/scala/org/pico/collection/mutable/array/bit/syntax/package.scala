package org.pico.collection.mutable.array.bit

package object syntax {
  implicit class LongBitArrayOps_2s8EdpV(val array: Array[Long]) extends AnyVal {
    final def setAtIndex(i: Long, v: Long): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Long = array(i.toInt)

    final def long(n: Long, v: Put[Long]): Unit = {
      val i: Int = (n / 64).toInt
      val o: Int = (n % 64).toInt

      if (o == 0) {
        setAtIndex(i, v.value)
      } else {
        val p = 64 - o

        setAtIndex(i + 0, getAtIndex(i + 0) >>> p << p | (v.value >>> o))
        setAtIndex(i + 1, getAtIndex(i + 1) << o >>> o | (v.value <<  p))
      }
    }

    final def long(n: Long): Long = {
      val i: Int = (n / 64).toInt
      val o: Int = (n % 64).toInt

      if (o == 0) {
        getAtIndex(i)
      } else {
        val p = 64 - o

        getAtIndex(i + 0) << o | getAtIndex(i + 1) >>> p
      }
    }
  }

  implicit class IntBitArrayOps_2s8EdpV(val array: Array[Int]) extends AnyVal {
    final def setAtIndex(i: Long, v: Int): Unit = array(i.toInt) = v

    final def getAtIndex(i: Long): Int = array(i.toInt)

    final def int(n: Long, v: Put[Int]): Unit = {
      val i: Int = (n / 32).toInt
      val o: Int = (n % 32).toInt

      if (o == 0) {
        setAtIndex(i, v.value)
      } else {
        val p = 32 - o

        setAtIndex(i + 0, getAtIndex(i + 0) >>> p << p | (v.value >>> o))
        setAtIndex(i + 1, getAtIndex(i + 1) << o >>> o | (v.value <<  p))
      }
    }

    final def int(n: Long): Int = {
      val i: Int = (n / 32).toInt
      val o: Int = (n % 32).toInt

      if (o == 0) {
        getAtIndex(i)
      } else {
        val p: Int = (32 - o)

        getAtIndex(i + 0) << o | getAtIndex(i + 1) >>> p
      }
    }
  }
}
