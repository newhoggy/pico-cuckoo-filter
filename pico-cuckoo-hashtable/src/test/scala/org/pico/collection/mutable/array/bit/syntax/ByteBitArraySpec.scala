package org.pico.collection.mutable.array.bit.syntax

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.util.Random

class ByteBitArraySpec extends Specification with ScalaCheck {
  case class Offset(value: Long)

  implicit val arbitraryOffset = Arbitrary[Offset](Gen.choose(0L, 128L).map(Offset))

  def arbitraryBytes(size: Int) = Arbitrary[Array[Byte]] {
    Gen.wrap(Gen.listOfN(size, Gen.wrap(Gen.const(Random.nextInt.toByte))).map(_.toArray))
  }

  def arbitraryShorts(size: Int) = Arbitrary[Array[Short]] {
    Gen.wrap(Gen.listOfN(size, Gen.wrap(Gen.const(Random.nextInt.toShort))).map(_.toArray))
  }

  "Bytes that are set can be retrieved again" in {
    prop { (v: Byte, offset: Offset, buffer: Array[Byte]) =>
      buffer.size ==== 130
      offset.value must be_>=(0L)
      offset.value must be_<(128L)
      buffer.byte(offset.value, Put(v))
      buffer.byte(offset.value) ==== v
    }.setArbitrary3(arbitraryBytes(130))
  }

  "Shorts that are set can be retrieved again" in {
    prop { (v: Short, offset: Offset, buffer: Array[Short]) =>
      buffer.size ==== 65
      offset.value must be_>=(0L)
      offset.value must be_<(128L)
      buffer.short(offset.value, Put(v))
      buffer.short(offset.value) ==== v
    }.setArbitrary3(arbitraryShorts(65))
  }
}
