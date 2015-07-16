package org.pico.collection.mutable.array.bit.syntax

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class IntBitArraySpec extends Specification with ScalaCheck {
  case class Offset(value: Long)

  implicit val arbitraryOffset = Arbitrary[Offset](Gen.choose(0L, 128L).map(Offset))

  "Values that are set can be retrieved again" in {
    prop { (v: Int, offset: Offset) =>
      val buffer = new Array[Int](10)
      buffer.int(offset.value, Put(v))
      buffer.int(offset.value) ==== v
    }
  }
}
