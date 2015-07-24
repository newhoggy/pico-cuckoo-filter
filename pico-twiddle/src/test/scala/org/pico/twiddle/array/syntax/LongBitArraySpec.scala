package org.pico.twiddle.array.syntax

import org.pico.twiddle.syntax.anyVal._
import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class LongBitArraySpec extends Specification with ScalaCheck {
//  "Bytes that are set can be retrieved again" in {
//    prop { (v: Byte, offset: Long, wallpaper: Long) =>
//      val buffer = Array.fill(65)(wallpaper)
//      offset must be_>=(0L)
//      offset must be_<(128L)
//      buffer.byte(offset, v)
//      buffer.byte(offset) ==== v
//    }.setGen2(Gen.choose(0L, 127L))
//  }
//
//  "Shorts that are set can be retrieved again" in {
//    prop { (v: Short, offset: Long, wallpaper: Long) =>
//      val buffer = Array.fill(65)(wallpaper)
//      offset must be_>=(0L)
//      offset must be_<(128L)
//      buffer.short(offset, v)
//      buffer.short(offset) ==== v
//    }.setGen2(Gen.choose(0L, 127L))
//  }
//
//  "Ints that are set can be retrieved again" in {
//    prop { (v: Int, offset: Long, wallpaper: Long) =>
//      val buffer = Array.fill(65)(wallpaper)
//      offset must be_>=(0L)
//      offset must be_<(128L)
//      buffer.int(offset, v)
//      buffer.int(offset) ==== v
//    }.setGen2(Gen.choose(0L, 127L))
//  }
//
//  "Longs that are set can be retrieved again" in {
//    prop { (v: Long, offset: Long, wallpaper: Long) =>
//      val buffer = Array.fill(65)(wallpaper)
//      offset must be_>=(0L)
//      offset must be_<(128L)
//      buffer.long(offset, v)
//      buffer.long(offset) ==== v
//    }.setGen2(Gen.choose(0L, 127L))
//  }
//
//  "Consecutive Bytes that are set can be retrieved again" in {
//    prop { (wallpaper: Long, offset: Long, u: Byte, v: Byte, w: Byte) =>
//      val buffer = Array.fill(130)(wallpaper)
//      offset must be_>=(0L)
//      offset must be_<(128L)
//      buffer.byte(offset + u.bitSize * 0, u)
//      buffer.byte(offset + w.bitSize * 2, w)
//      buffer.byte(offset + v.bitSize * 1, v)
//      buffer.byte(offset + u.bitSize * 0) ==== u
//      buffer.byte(offset + w.bitSize * 2) ==== w
//      buffer.byte(offset + v.bitSize * 1) ==== v
//    }.setGen2(Gen.choose(0L, 127L))
//  }
//
//  "Consecutive Shorts that are set can be retrieved again" in {
//    prop { (wallpaper: Long, offset: Long, u: Short, v: Short, w: Short) =>
//      val buffer = Array.fill(130)(wallpaper)
//      offset must be_>=(0L)
//      offset must be_<(128L)
//      buffer.short(offset + u.bitSize * 0, u)
//      buffer.short(offset + w.bitSize * 2, w)
//      buffer.short(offset + v.bitSize * 1, v)
//      buffer.short(offset + u.bitSize * 0) ==== u
//      buffer.short(offset + w.bitSize * 2) ==== w
//      buffer.short(offset + v.bitSize * 1) ==== v
//    }.setGen2(Gen.choose(0L, 127L))
//  }
//
//  "Consecutive Ints that are set can be retrieved again" in {
//    prop { (wallpaper: Long, offset: Long, u: Int, v: Int, w: Int) =>
//      val buffer = Array.fill(130)(wallpaper)
//      offset must be_>=(0L)
//      offset must be_<(128L)
//      buffer.int(offset + u.bitSize * 0, u)
//      buffer.int(offset + w.bitSize * 2, w)
//      buffer.int(offset + v.bitSize * 1, v)
//      buffer.int(offset + u.bitSize * 0) ==== u
//      buffer.int(offset + w.bitSize * 2) ==== w
//      buffer.int(offset + v.bitSize * 1) ==== v
//    }.setGen2(Gen.choose(0L, 127L))
//  }
//
//  "Consecutive Longs that are set can be retrieved again" in {
//    prop { (wallpaper: Long, offset: Long, u: Long, v: Long, w: Long) =>
//      val buffer = Array.fill(130)(wallpaper)
//      offset must be_>=(0L)
//      offset must be_<(128L)
//      buffer.long(offset + u.bitSize * 0, u)
//      buffer.long(offset + w.bitSize * 2, w)
//      buffer.long(offset + v.bitSize * 1, v)
//      buffer.long(offset + u.bitSize * 0) ==== u
//      buffer.long(offset + w.bitSize * 2) ==== w
//      buffer.long(offset + v.bitSize * 1) ==== v
//    }.setGen2(Gen.choose(0L, 127L))
//  }

  "Consecutive Bytes that are set on saturated wallpaper can be retrieved again" in {
    for (offset <- 0L until 128L) {
      val wallpaper = -1L
      val u = 0x42.toByte
      val buffer = Array.fill(130)(wallpaper)
      buffer.byte(offset + u.bitSize * 0, u)
      buffer.byte(offset + u.bitSize * 0) ==== u
    }

    success
  }

//  "Consecutive Shorts that are set on saturated wallpaper can be retrieved again" in {
//    for (offset <- 0L until 128L) {
//      val wallpaper = -1L
//      val u = 0x4002.toShort
//      val buffer = Array.fill(130)(wallpaper)
//      buffer.short(offset + u.bitSize * 0, u)
//      buffer.short(offset + u.bitSize * 0) ==== u
//    }
//
//    success
//  }
}
