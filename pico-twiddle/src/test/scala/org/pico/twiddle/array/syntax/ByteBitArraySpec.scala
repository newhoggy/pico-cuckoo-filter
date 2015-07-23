package org.pico.twiddle.array.syntax

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class ByteBitArraySpec extends Specification with ScalaCheck {
  sequential

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
      buffer.byte(offset + 8 * 0, u)
      buffer.byte(offset + 8 * 2, w)
      buffer.byte(offset + 8 * 1, v)
      buffer.byte(offset + 8 * 0) ==== u
      buffer.byte(offset + 8 * 2) ==== w
      buffer.byte(offset + 8 * 1) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Consecutive Shorts that are set can be retrieved again" in {
    prop { (wallpaper: Byte, offset: Long, u: Short, v: Short, w: Short) =>
      val buffer = Array.fill(130)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.short(offset + 16 * 0, u)
      buffer.short(offset + 16 * 2, w)
      buffer.short(offset + 16 * 1, v)
      buffer.short(offset + 16 * 0) ==== u
      buffer.short(offset + 16 * 2) ==== w
      buffer.short(offset + 16 * 1) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

//  "Consecutive Ints that are set can be retrieved again" in {
//    prop { (wallpaper: Byte, offset: Long, u: Int, v: Int, w: Int) =>
//      val buffer = Array.fill(130)(wallpaper)
//      offset must be_>=(0L)
//      offset must be_<(128L)
//      buffer.int(offset + 8 * 0, u)
//      buffer.int(offset + 8 * 2, w)
//      buffer.int(offset + 8 * 1, v)
//      buffer.int(offset + 8 * 0) ==== u
//      buffer.int(offset + 8 * 2) ==== w
//      buffer.int(offset + 8 * 1) ==== v
//    }.setGen2(Gen.choose(0L, 127L))
//  }
//
//  "Consecutive Longs that are set can be retrieved again" in {
//    prop { (wallpaper: Byte, offset: Long, u: Long, v: Long, w: Long) =>
//      val buffer = Array.fill(130)(wallpaper)
//      offset must be_>=(0L)
//      offset must be_<(128L)
//      buffer.long(offset + 8 * 0, u)
//      buffer.long(offset + 8 * 2, w)
//      buffer.long(offset + 8 * 1, v)
//      buffer.long(offset + 8 * 0) ==== u
//      buffer.long(offset + 8 * 2) ==== w
//      buffer.long(offset + 8 * 1) ==== v
//    }.setGen2(Gen.choose(0L, 127L))
//  }
}
