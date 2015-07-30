package org.pico.twiddle.syntax.array

import org.pico.twiddle.instances._
import org.pico.twiddle.syntax.anyVal._
import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class IntBitArraySpec extends Specification with ScalaCheck {
  sequential

  "Bytes that are set can be retrieved again" in {
    prop { (v: Byte, offset: Long, wallpaper: Int) =>
      val buffer = Array.fill(65)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.int(offset.bits, v)
      buffer.int(offset.bits) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Shorts that are set can be retrieved again" in {
    prop { (v: Short, offset: Long, wallpaper: Int) =>
      val buffer = Array.fill(65)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.int(offset.bits, v)
      buffer.int(offset.bits) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Ints that are set can be retrieved again" in {
    prop { (v: Int, offset: Long, wallpaper: Int) =>
      val buffer = Array.fill(65)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.int(offset.bits, v)
      buffer.int(offset.bits) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Longs that are set can be retrieved again" in {
    prop { (v: Long, offset: Long, wallpaper: Int) =>
      val buffer = Array.fill(65)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.long(offset.bits, v)
      buffer.long(offset.bits) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Consecutive Bytes that are set can be retrieved again" in {
    prop { (wallpaper: Int, offset: Long, u: Byte, v: Byte, w: Byte) =>
      val buffer = Array.fill(130)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.byte(offset.bits + u.bitSize * 0, u)
      buffer.byte(offset.bits + w.bitSize * 2, w)
      buffer.byte(offset.bits + v.bitSize * 1, v)
      buffer.byte(offset.bits + u.bitSize * 0) ==== u
      buffer.byte(offset.bits + w.bitSize * 2) ==== w
      buffer.byte(offset.bits + v.bitSize * 1) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Consecutive Shorts that are set can be retrieved again" in {
    prop { (wallpaper: Int, offset: Long, u: Short, v: Short, w: Short) =>
      val buffer = Array.fill(130)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.short(offset.bits + u.bitSize * 0, u)
      buffer.short(offset.bits + w.bitSize * 2, w)
      buffer.short(offset.bits + v.bitSize * 1, v)
      buffer.short(offset.bits + u.bitSize * 0) ==== u
      buffer.short(offset.bits + w.bitSize * 2) ==== w
      buffer.short(offset.bits + v.bitSize * 1) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Consecutive Ints that are set can be retrieved again" in {
    prop { (wallpaper: Int, offset: Long, u: Int, v: Int, w: Int) =>
      val buffer = Array.fill(130)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.int(offset.bits + u.bitSize * 0, u)
      buffer.int(offset.bits + w.bitSize * 2, w)
      buffer.int(offset.bits + v.bitSize * 1, v)
      buffer.int(offset.bits + u.bitSize * 0) ==== u
      buffer.int(offset.bits + w.bitSize * 2) ==== w
      buffer.int(offset.bits + v.bitSize * 1) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Consecutive Longs that are set can be retrieved again" in {
    prop { (wallpaper: Int, offset: Long, u: Long, v: Long, w: Long) =>
      val buffer = Array.fill(130)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.long(offset.bits + u.bitSize * 0, u)
      buffer.long(offset.bits + w.bitSize * 2, w)
      buffer.long(offset.bits + v.bitSize * 1, v)
      buffer.long(offset.bits + u.bitSize * 0) ==== u
      buffer.long(offset.bits + w.bitSize * 2) ==== w
      buffer.long(offset.bits + v.bitSize * 1) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Consecutive Bytes that are set on saturated wallpaper can be retrieved again" in {
    for (o <- 0.bits until 128.bits) {
      val wallpaper = -1
      val u = 0x42.toByte
      val buffer = Array.fill(130)(wallpaper)
      buffer.byte(o + u.bitSize * 0, u)
      buffer.byte(o + u.bitSize * 0) ==== u
    }

    success
  }

  "Consecutive Shorts that are set on saturated wallpaper can be retrieved again" in {
    val wallpaper = -1
    val u = 0x4002.toShort
    for (o <- 0.bits until 128.bits) {
      val buffer = Array.fill(130)(wallpaper)
      buffer.short(o, u)
      buffer.short(o) ==== u
    }

    success
  }

  "Consecutive Int that are set on saturated wallpaper can be retrieved again" in {
    for (o <- 0.bits until 128.bits) {
      val wallpaper = -1
      val u = 0x40000002
      val buffer = Array.fill(130)(wallpaper)
      buffer.int(o + u.bitSize * 0, u)
      buffer.int(o + u.bitSize * 0) ==== u
    }

    success
  }

  "Consecutive Long that are set on saturated wallpaper can be retrieved again" in {
    val wallpaper = -1
    val u = 0x4000000000000002L
    for (o <- 0.bits until 128.bits) {
      val p = o + u.bitSize
      val buffer = Array.fill(130)(wallpaper)
      buffer.long(o + u.bitSize * 0, u)
      buffer.long(o + u.bitSize * 0) ==== u
    }

    success
  }

  "Consecutive Shorts that are set on saturated wallpaper can be retrieved again" in {
    val wallpaper = -1
    val u = 0x4002.toShort
    for (o <- 0.bits until 128.bits) {
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
    val wallpaper = -1
    val u = 0x42.toByte

    for (o <- 0.bits until 128.bits) {
      val buffer = Array.fill(130)(wallpaper)
      buffer.update(o, 8.bits, u)
      buffer.byte(o) ==== u
    }

    success
  }

  "Setting unsigned int of size 16 is equivalent to setting short" in {
    val wallpaper = -1
    val u = 0x4002.toShort

    for (o <- 0.bits until 128.bits) {
      val buffer = Array.fill(130)(wallpaper)
      buffer.update(o, 16.bits, u)
      buffer.short(o) ==== u
    }

    success
  }

  "Setting unsigned int of size 32 is equivalent to setting int" in {
    val wallpaper = -1
    val u = 0x40000002

    for (o <- 0.bits until 128.bits) {
      val buffer = Array.fill(130)(wallpaper)
      buffer.update(o, 32.bits, u)
      buffer.int(o) ==== u
    }

    success
  }

  "Setting unsigned int of size 64 is equivalent to setting long" in {
    val wallpaper = -1
    val u = 0x4000000000000002L

    for (o <- 0.bits until 128.bits) {
      val buffer = Array.fill(130)(wallpaper)
      buffer.update(o, 64.bits, u)
      buffer.long(o) ==== u
    }

    success
  }
}
