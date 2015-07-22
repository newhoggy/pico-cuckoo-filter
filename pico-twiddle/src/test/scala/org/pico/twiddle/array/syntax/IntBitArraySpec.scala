package org.pico.twiddle.array.syntax

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class IntBitArraySpec extends Specification with ScalaCheck {
  "Bytes that are set can be retrieved again" in {
    prop { (v: Byte, offset: Long, wallpaper: Int) =>
      val buffer = Array.fill(65)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.int(offset, v)
      buffer.int(offset) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Shorts that are set can be retrieved again" in {
    prop { (v: Short, offset: Long, wallpaper: Int) =>
      val buffer = Array.fill(65)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.int(offset, v)
      buffer.int(offset) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Ints that are set can be retrieved again" in {
    prop { (v: Int, offset: Long, wallpaper: Int) =>
      val buffer = Array.fill(65)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.int(offset, v)
      buffer.int(offset) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Longs that are set can be retrieved again" in {
    prop { (v: Long, offset: Long, wallpaper: Int) =>
      val buffer = Array.fill(65)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.long(offset, v)
      buffer.long(offset) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }
}
