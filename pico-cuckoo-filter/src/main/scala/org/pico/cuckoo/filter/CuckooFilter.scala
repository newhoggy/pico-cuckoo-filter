package org.pico.cuckoo.filter

import java.lang.{Integer => JInteger}

import org.pico.hash.{Hash64, Hashable}
import org.pico.hash.syntax._
import org.pico.twiddle.Bits
import org.pico.twiddle.instances._
import org.pico.twiddle.syntax.arrayIndexed._
import org.pico.twiddle.syntax.anyVal._
import org.pico.twiddle.syntax.fixedInt._

import scala.annotation.tailrec
import scala.util.Random

class CuckooFilter(fingerprintsPerBucket: Int, fingerprintBits: Bits, maxNumKicks: Int = 5, totalBuckets: Int = 128) {
  require(fingerprintBits > Bits(0))
  require(maxNumKicks > 0)
  require(fingerprintsPerBucket > 0)

  type DI = DummyImplicit

  private val bucketIndexBits = Bits(32 - JInteger.numberOfLeadingZeros(fingerprintsPerBucket))

  private val bucketBits = Bits((1 << fingerprintBits) * fingerprintsPerBucket) + bucketIndexBits

  private val buffer = new Array[Byte]((bucketBits * totalBuckets) /+ bitSize[Byte])

  def bucketBitIndex(index: Long): Bits = bucketBits * index

  def bucketBitIndex(hash: Hash64)(implicit ev: DummyImplicit): Bits = {
    bucketBitIndex((hash.value & 0x7fffffffffffffffL) % totalBuckets)
  }

  def fingerprintsInBucketAt(bucketBitIndex: Bits): Int = buffer.unsigned(bucketBitIndex, bucketIndexBits).toInt

  def fingerprintsInBucketAt(bucketBitIndex: Bits, value: Long): Unit = {
    buffer.update(bucketBitIndex, bucketIndexBits, value)
  }

  def setFingerprint(bucketBitIndex: Bits, fingerprintIndex: Int, fingerprint: Fingerprint): Unit = {
    buffer.update(
      bucketBitIndex + bucketIndexBits + fingerprintBits * fingerprintIndex, fingerprintBits, fingerprint.value)
  }

  def getFingerprint(bucketBitIndex: Bits, fingerprintIndex: Int): Fingerprint = {
    Fingerprint(
      buffer.unsigned(bucketBitIndex + bucketIndexBits + fingerprintBits * fingerprintIndex, fingerprintBits))
  }

  def bucketBits(bucketBitIndex: Bits): String = buffer.bitsString(bucketBitIndex, bucketBits)

  def removeFingerprintFromBucketForHash(bucketBitIndex: Bits, f: Fingerprint): Boolean = {
    val fingerprints = fingerprintsInBucketAt(bucketBitIndex)
    val index = fingerprintIndex(bucketBitIndex, f)

    if (index != -1) {
      setFingerprint(bucketBitIndex, index, getFingerprint(bucketBitIndex, fingerprints - 1))
      setFingerprint(bucketBitIndex, fingerprints - 1, Fingerprint(0))
      true
    } else {
      false
    }
  }

  def fingerprintIndex(bucketBitIndex: Bits, fingerprint: Fingerprint): Int = {
    val fingerprints = fingerprintsInBucketAt(bucketBitIndex)

    @tailrec def go(index: Int): Int = {
      if (index < fingerprints) {
        val f = getFingerprint(bucketBitIndex, index)

        if (f == fingerprint) {
          index
        } else {
          go(index + 1)
        }
      } else {
        -1
      }
    }

    go(0)
  }

  def fingerprintIsInBucket(bucketBitIndex: Bits, fingerprint: Fingerprint): Boolean = {
    fingerprintIndex(bucketBitIndex, fingerprint) != -1
  }

  def fingerprint[A: Hashable](a: A): Fingerprint = Fingerprint(implicitly[Hashable[A]].hash(a).value)

  def addToBucket(bucketBitIndex: Bits, f: Fingerprint): Boolean = {
    val fingerprints = fingerprintsInBucketAt(bucketBitIndex)

    if (fingerprints < fingerprintsPerBucket) {
      setFingerprint(bucketBitIndex, fingerprints, f)
      fingerprintsInBucketAt(bucketBitIndex, fingerprints + 1)
      true
    } else {
      false
    }
  }

  def swapRandomBucketEntry(bucketBitIndex: Bits, f: Fingerprint): Fingerprint = {
    val fingerprints = fingerprintsInBucketAt(bucketBitIndex)

    if (fingerprints > 0) {
      val candidateIndex = Random.nextInt(fingerprints)
      val candidate = getFingerprint(bucketBitIndex, candidateIndex)
      setFingerprint(bucketBitIndex, candidateIndex, f)
      candidate
    } else {
      f
    }
  }

  final def insert[A: Hashable](value: A)(implicit ev0: Hashable[Long], ev1: Hashable[Fingerprint]): Boolean = {
    var f = fingerprint(value)
    val i1 = value.hashed
    val i2 = i1 ^ f.hashed

    addToBucket(bucketBitIndex(i1), f) || addToBucket(bucketBitIndex(i2), f) || {
      // must relocate existing items
      var i = if (Random.nextBoolean()) i1 else i2

      for (n <- 0 until maxNumKicks) {
        f = swapRandomBucketEntry(bucketBitIndex(i), f)

        i = i ^ f.hashed

        if (addToBucket(bucketBitIndex(i), f)) {
          return true
        }
      }

      false
    }
  }

  final def lookup[A: Hashable](value: A)(implicit ev0: Hashable[Long], ev1: Hashable[Fingerprint]): Boolean = {
    val f = fingerprint(value)
    val i1 = value.hashed
    val i2 = i1 ^ f.hashed

    fingerprintIsInBucket(bucketBitIndex(i1), f) || fingerprintIsInBucket(bucketBitIndex(i2), f)
  }

  final def delete[A: Hashable](value: A)(implicit ev0: Hashable[Long], ev1: Hashable[Fingerprint]): Boolean = {
    val f = fingerprint(value)
    val i1 = value.hashed
    val i2 = i1 ^ f.hashed

    removeFingerprintFromBucketForHash(bucketBitIndex(i1), f) || removeFingerprintFromBucketForHash(bucketBitIndex(i2), f)
  }
}
