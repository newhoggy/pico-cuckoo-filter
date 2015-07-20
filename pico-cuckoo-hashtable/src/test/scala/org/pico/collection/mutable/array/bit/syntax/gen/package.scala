package org.pico.collection.mutable.array.bit.syntax

import org.scalacheck.Gen

import scala.util.Random

package object gen {
  def genBytes(size: Int) = Gen.wrap(Gen.listOfN(size, Gen.wrap(Gen.const(Random.nextInt().toByte))))

  def genShorts(size: Int) = Gen.wrap(Gen.listOfN(size, Gen.wrap(Gen.const(Random.nextInt().toShort))))

  def genInts(size: Int) = Gen.wrap(Gen.listOfN(size, Gen.wrap(Gen.const(Random.nextInt()))))

  def genLongs(size: Int) = Gen.wrap(Gen.listOfN(size, Gen.wrap(Gen.const(Random.nextLong()))))
}
