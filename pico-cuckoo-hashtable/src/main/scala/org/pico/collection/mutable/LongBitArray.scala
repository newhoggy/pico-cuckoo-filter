package org.pico.collection.mutable

case class LongBitArray(array: Array[Long]) extends AnyVal {
  final def setAtIndex(i: Long, v: Long): Unit = array(i.toInt) = v

  final def getAtIndex(i: Long): Long = array(i.toInt)

  final def update(n: Long, v: Long): Unit = {
    val i: Long = n / 64
    val o: Long = n % 64

    if (o == 0) {
      setAtIndex(i, v)
    } else {
      val p = 64 - o

      setAtIndex(i + 0, getAtIndex(i + 0) >>> p << p | (v >>> o))
      setAtIndex(i + 1, getAtIndex(i + 1) << o >>> o | (v <<  p))
    }
  }

  final def apply(n: Long): Long = {
    val i: Long = n / 64
    val o: Long = n % 64

    if (o == 0) {
      getAtIndex(i)
    } else {
      val p = 64 - o

      getAtIndex(i + 0) << o | getAtIndex(i + 1) >>> p
    }
  }
}
