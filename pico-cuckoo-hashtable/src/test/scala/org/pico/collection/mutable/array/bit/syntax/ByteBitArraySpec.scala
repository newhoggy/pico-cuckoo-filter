package org.pico.collection.mutable.array.bit.syntax

import org.pico.collection.mutable.array.bit.syntax.gen._
import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class ByteBitArraySpec extends Specification with ScalaCheck {
  "Bytes that are set can be retrieved again" in {
    prop { (v: Byte, offset: Long, bytes: List[Byte]) =>
      val buffer = bytes.toArray
      buffer.length ==== 130
      offset must be_>=(0L)
      offset must be_<(128L)
      buffer.byte(offset, Put(v))
      buffer.byte(offset) ==== v
    }.setGen3(genBytes(130)).setGen2(Gen.choose(0L, 127L))
  }

  "Shorts that are set can be retrieved again" in {
    prop { (v: Short, offset: Long, bytes: List[Byte]) =>
      val offset = 1L
      val buffer = bytes.toArray
      buffer.short(offset, Put(v))
      buffer.short(offset) ==== v
    }.setGen2(Gen.choose(0L, 127L)).setGen3(genBytes(130))
  }
}
