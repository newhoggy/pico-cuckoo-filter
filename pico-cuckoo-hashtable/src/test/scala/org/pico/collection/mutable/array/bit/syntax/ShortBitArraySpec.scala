package org.pico.collection.mutable.array.bit.syntax

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class ShortBitArraySpec extends Specification with ScalaCheck {
  "Values that are set can be retrieved again" in {
    prop { (v: Short, offset: Long) =>
      val buffer = new Array[Short](10)
      buffer.short(offset, Put(v))
      buffer.short(offset) ==== v
    }.setGen2(Gen.choose(0L, 128L))
  }
}
