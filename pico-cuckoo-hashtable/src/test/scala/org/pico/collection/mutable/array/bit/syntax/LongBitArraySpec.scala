package org.pico.collection.mutable.array.bit.syntax

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class LongBitArraySpec extends Specification with ScalaCheck {
  "Bytes that are set can be retrieved again" in {
    prop { (v: Long, offset: Long) =>
      val buffer = new Array[Long](5)
      buffer.long(offset, Put(v))
      buffer.long(offset) ==== v
    }.setGen2(Gen.choose(0L, 128L))
  }
}
