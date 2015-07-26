package org.pico.twiddle

trait ArrayIndexed[A, @specialized(Byte, Short, Int, Long) E] {
  def elemBitSize: Int

  def setAtIndex(indexed: A, i: Long, v: E): Unit

  def getAtIndex(indexed: A, i: Long): E
}
