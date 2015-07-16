package org.pico.collection.mutable.array.bit.syntax

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class LongBitArraySpec extends Specification with ScalaCheck {
  case class Offset(value: Long)

  implicit val arbitraryOffset = Arbitrary[Offset](Gen.choose(0L, 128L).map(Offset))

  "Bytes that are set can be retrieved again" in {
    prop { (v: Long, offset: Offset) =>
      val buffer = new Array[Long](5)
      buffer.long(offset.value, Put(v))
      buffer.long(offset.value) ==== v
    }
  }
}
