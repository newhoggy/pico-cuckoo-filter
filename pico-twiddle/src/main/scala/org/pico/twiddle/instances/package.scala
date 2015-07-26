package org.pico.twiddle

package object instances {
  implicit val arrayIndexedByteArray = new ArrayIndexed[Array[Byte], Byte] {
    override def setAtIndex(indexed: Array[Byte], i: Long, v: Byte): Unit = indexed(i.toInt) = v

    override def getAtIndex(indexed: Array[Byte], i: Long): Byte = indexed(i.toInt)
  }

  implicit val arrayIndexedShortArray = new ArrayIndexed[Array[Short], Short] {
    override def setAtIndex(indexed: Array[Short], i: Long, v: Short): Unit = indexed(i.toInt) = v

    override def getAtIndex(indexed: Array[Short], i: Long): Short = indexed(i.toInt)
  }

  implicit val arrayIndexedIntArray = new ArrayIndexed[Array[Int], Int] {
    override def setAtIndex(indexed: Array[Int], i: Long, v: Int): Unit = indexed(i.toInt) = v

    override def getAtIndex(indexed: Array[Int], i: Long): Int = indexed(i.toInt)
  }

  implicit val arrayIndexedLongArray = new ArrayIndexed[Array[Long], Long] {
    override def setAtIndex(indexed: Array[Long], i: Long, v: Long): Unit = indexed(i.toInt) = v

    override def getAtIndex(indexed: Array[Long], i: Long): Long = indexed(i.toInt)
  }
}
