package org.pico.collection.mutable.array.bit.syntax

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class ShortBitArraySpec extends Specification with ScalaCheck {
  "Shorts that are set can be retrieved again" in {
    prop { (v: Short, offset: Long, wallpaper: Short) =>
      val buffer = Array.fill(65)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.int(offset, Put(v))
      buffer.int(offset) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }

  "Ints that are set can be retrieved again" in {
    prop { (v: Int, offset: Long, wallpaper: Short) =>
      val buffer = Array.fill(65)(wallpaper)
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.int(offset, Put(v))
      buffer.int(offset) ==== v
    }.setGen2(Gen.choose(0L, 127L))
  }
}
