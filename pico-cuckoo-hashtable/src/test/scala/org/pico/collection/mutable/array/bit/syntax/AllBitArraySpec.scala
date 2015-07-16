package org.pico.collection.mutable.array.bit.syntax

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class AllBitArraySpec extends Specification with ScalaCheck {
  case class Offset(value: Long)

  implicit val arbitraryOffset = Arbitrary[Offset](Gen.choose(0L, 128L).map(Offset))

  "Values that are set can be retrieved again" in {
    prop { (v: Byte, offset: Offset) =>
      val b64 = new Array[Byte]( 32)
      val b32 = new Array[Byte]( 64)
      val b16 = new Array[Byte](128)
      val b08 = new Array[Byte](256)

      b64.byte(offset.value, Put(v))
      b32.byte(offset.value, Put(v))
      b16.byte(offset.value, Put(v))
      b08.byte(offset.value, Put(v))

      success
    }
  }
}
