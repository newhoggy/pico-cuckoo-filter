package org.pico.collection.mutable.array.bit.syntax

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class AllBitArraySpec extends Specification with ScalaCheck {
  "Values that are set can be retrieved again" in {
    prop { (v: Byte, offset: Long) =>
      val b64 = new Array[Byte]( 32)
      val b32 = new Array[Byte]( 64)
      val b16 = new Array[Byte](128)
      val b08 = new Array[Byte](256)

      b64.byte(offset, Put(v))
      b32.byte(offset, Put(v))
      b16.byte(offset, Put(v))
      b08.byte(offset, Put(v))

      success
    }.setGen2(Gen.choose(0L, 127L))
  }
}
