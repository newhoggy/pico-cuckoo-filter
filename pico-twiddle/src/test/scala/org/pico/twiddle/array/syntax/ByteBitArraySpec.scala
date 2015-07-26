package org.pico.twiddle.array.syntax

import org.pico.twiddle.syntax.anyVal._
import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class ByteBitArraySpec extends Specification with ScalaCheck {
  "Bytes that are set can be retrieved again" in {
    prop { (v: Byte, offset: Long, wallpaper: Byte) =>
      val buffer = Array.fill(130)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.byte(offset, v)
      buffer.byte(offset) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Shorts that are set can be retrieved again" in {
    prop { (v: Short, offset: Long, wallpaper: Byte) =>
      val buffer = Array.fill(130)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.short(offset, v)
      buffer.short(offset) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Ints that are set can be retrieved again" in {
    prop { (v: Int, offset: Long, wallpaper: Byte) =>
      val buffer = Array.fill(130)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.int(offset, v)
      buffer.int(offset) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Longs that are set can be retrieved again" in {
    prop { (v: Long, offset: Long, wallpaper: Byte) =>
      val buffer = Array.fill(130)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.long(offset, v)
      buffer.long(offset) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Consecutive Bytes that are set can be retrieved again" in {
    prop { (wallpaper: Byte, offset: Long, u: Byte, v: Byte, w: Byte) =>
      val buffer = Array.fill(130)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.byte(offset + u.bitSize * 0, u)
      buffer.byte(offset + w.bitSize * 2, w)
      buffer.byte(offset + v.bitSize * 1, v)
      buffer.byte(offset + u.bitSize * 0) ==== u
      buffer.byte(offset + w.bitSize * 2) ==== w
      buffer.byte(offset + v.bitSize * 1) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Consecutive Shorts that are set can be retrieved again" in {
    prop { (wallpaper: Byte, offset: Long, u: Short, v: Short, w: Short) =>
      val buffer = Array.fill(130)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.short(offset + u.bitSize * 0, u)
      buffer.short(offset + w.bitSize * 2, w)
      buffer.short(offset + v.bitSize * 1, v)
      buffer.short(offset + u.bitSize * 0) ==== u
      buffer.short(offset + w.bitSize * 2) ==== w
      buffer.short(offset + v.bitSize * 1) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Consecutive Ints that are set can be retrieved again" in {
    prop { (wallpaper: Byte, offset: Long, u: Int, v: Int, w: Int) =>
      val buffer = Array.fill(130)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.int(offset + u.bitSize * 0, u)
      buffer.int(offset + w.bitSize * 2, w)
      buffer.int(offset + v.bitSize * 1, v)
      buffer.int(offset + u.bitSize * 0) ==== u
      buffer.int(offset + w.bitSize * 2) ==== w
      buffer.int(offset + v.bitSize * 1) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Consecutive Longs that are set can be retrieved again" in {
    prop { (wallpaper: Byte, offset: Long, u: Long, v: Long, w: Long) =>
      val buffer = Array.fill(130)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.long(offset + u.bitSize * 0, u)
      buffer.long(offset + w.bitSize * 2, w)
      buffer.long(offset + v.bitSize * 1, v)
      buffer.long(offset + u.bitSize * 0) ==== u
      buffer.long(offset + w.bitSize * 2) ==== w
      buffer.long(offset + v.bitSize * 1) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Consecutive Bytes that are set on saturated wallpaper can be retrieved again" in {
    for (offset <- 0L until 128L) {
      val wallpaper = -1.toByte
      val u = 0x42.toByte
      val buffer = Array.fill(130)(wallpaper)
      buffer.byte(offset + u.bitSize * 0, u)
      buffer.byte(offset + u.bitSize * 0) ==== u
    }

    success
  }

  "Consecutive Shorts that are set on saturated wallpaper can be retrieved again" in {
    val wallpaper = -1.toByte
    val u = 0x4002.toShort
    for (o <- 0L until 128L) {
      val buffer = Array.fill(130)(wallpaper)
      buffer.short(o, u)
      buffer.short(o) ==== u
    }

    success
  }

  "Consecutive Int that are set on saturated wallpaper can be retrieved again" in {
    for (offset <- 0L until 128L) {
      val wallpaper = -1.toByte
      val u = 0x40000002
      val buffer = Array.fill(130)(wallpaper)
      buffer.int(offset + u.bitSize * 0, u)
      buffer.int(offset + u.bitSize * 0) ==== u
    }

    success
  }

  "Consecutive Long that are set on saturated wallpaper can be retrieved again" in {
    val wallpaper = -1.toByte
    val u = 0x4000000000000002L
    for (o <- 0L until 128L) {
      val p = o + u.bitSize
      val buffer = Array.fill(130)(wallpaper)
      buffer.long(o + u.bitSize * 0, u)
      buffer.long(o + u.bitSize * 0) ==== u
    }

    success
  }

  "Consecutive Shorts that are set on saturated wallpaper can be retrieved again" in {
    val wallpaper = -1.toByte
    val u = 0x4002.toShort
    for (o <- 0L until 128L) {
      val p = o + u.bitSize
      val buffer = Array.fill(130)(wallpaper)
      buffer.short(o, u)
      buffer.short(p, u)
      buffer.short(o) ==== u
      buffer.short(p) ==== u
    }

    success
  }

  "Setting unsigned int of size 8 is equivalent to setting byte" in {
    val wallpaper = -1.toByte
    val u = 0x42.toByte

    for (o <- 0L until 128L) {
      val buffer = Array.fill(130)(wallpaper)
      buffer.update(o, 8, u)
      buffer.byte(o) ==== u
    }

    success
  }

  "Setting unsigned int of size 16 is equivalent to setting short" in {
    val wallpaper = -1.toByte
    val u = 0x4002.toShort

    for (o <- 0L until 128L) {
      val buffer = Array.fill(130)(wallpaper)
      buffer.update(o, 16, u)
      buffer.short(o) ==== u
    }

    success
  }

  "Setting unsigned int of size 32 is equivalent to setting int" in {
    val wallpaper = -1.toByte
    val u = 0x40000002

    for (o <- 0L until 128L) {
      val buffer = Array.fill(130)(wallpaper)
      buffer.update(o, 32, u)
      buffer.int(o) ==== u
    }

    success
  }

  "Setting unsigned int of size 64 is equivalent to setting long" in {
    val wallpaper = -1.toByte
    val u = 0x4000000000000002L

    for (o <- 0L until 128L) {
      val buffer = Array.fill(130)(wallpaper)
      buffer.update(o, 64, u)
      buffer.long(o) ==== u
    }

    success
  }
}
