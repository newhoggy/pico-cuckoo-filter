package org.pico.twiddle.array.syntax

import org.pico.twiddle.syntax.anyVal._
import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class ShortBitArraySpec extends Specification with ScalaCheck {
  "Bytes that are set can be retrieved again" in {
    prop { (v: Byte, offset: Long, wallpaper: Short) =>
      val buffer = Array.fill(65)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.int(offset, v)
      buffer.int(offset) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Shorts that are set can be retrieved again" in {
    prop { (v: Short, offset: Long, wallpaper: Short) =>
      val buffer = Array.fill(65)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.int(offset, v)
      buffer.int(offset) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Ints that are set can be retrieved again" in {
    prop { (v: Int, offset: Long, wallpaper: Short) =>
      val buffer = Array.fill(65)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.int(offset, v)
      buffer.int(offset) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Longs that are set can be retrieved again" in {
    prop { (v: Long, offset: Long, wallpaper: Short) =>
      val buffer = Array.fill(65)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.long(offset, v)
      buffer.long(offset) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Consecutive Bytes that are set can be retrieved again" in {
    prop { (wallpaper: Short, offset: Long, u: Byte, v: Byte, w: Byte) =>
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
    prop { (wallpaper: Short, offset: Long, u: Short, v: Short, w: Short) =>
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
    prop { (wallpaper: Short, offset: Long, u: Int, v: Int, w: Int) =>
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
    prop { (wallpaper: Short, offset: Long, u: Long, v: Long, w: Long) =>
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
}
