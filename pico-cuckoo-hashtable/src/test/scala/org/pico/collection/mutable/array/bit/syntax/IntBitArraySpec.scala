package org.pico.collection.mutable.array.bit.syntax

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class IntBitArraySpec extends Specification with ScalaCheck {
  "Values that are set can be retrieved again" in {
    prop { (v: Int, offset: Long) =>
      val buffer = new Array[Int](10)
      buffer.int(offset, Put(v))
      buffer.int(offset) ==== v
    }.setGen2(Gen.choose(0L, 128L))
  }
}
