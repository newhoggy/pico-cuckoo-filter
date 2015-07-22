package org.pico.collection.mutable.array.bit.syntax

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
}
