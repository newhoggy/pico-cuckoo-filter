package org.pico.collection.mutable

import java.lang.management.ManagementFactory

import com.sun.management.ThreadMXBean

case class BitBuffer(buffer: Array[Byte]) extends AnyVal {
  final def boolean(offset: Int): Boolean = (buffer(offset / 8) & (1 << (offset & 0x7))) != 0

  final def boolean(offset: Int, b: Boolean): Unit = (buffer(offset / 8) & (1 << (offset & 0x7))) != 0
}

object Offset {
  def main(args: Array[String]): Unit = {
    val mx = ManagementFactory.getThreadMXBean.asInstanceOf[ThreadMXBean]
    val tid = Thread.currentThread.getId

    for (i <- 1 to 10) {
      Thread.sleep(10)
      val a = mx.getThreadAllocatedBytes(tid)
      val b = mx.getThreadAllocatedBytes(tid)
      val c = mx.getThreadAllocatedBytes(tid)
      val d = mx.getThreadAllocatedBytes(tid)
      println(a)
      println(b)
      println(c)
      println(d)
    }
  }
}