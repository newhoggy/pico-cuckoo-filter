package org.pico.cuckoo.filter

import org.pico.hash.Hashable
import org.pico.hash.syntax._
import org.pico.twiddle.instances._
import org.pico.twiddle.syntax.arrayIndexed._
import scala.annotation.tailrec
import scala.util.Random
import java.lang.{Integer => JInteger}

class CuckooFilter(fingerprintsPerBucket: Int, fingerprintBits: Int, maxNumKicks: Int = 5, totalBuckets: Int = 128) {
  require(fingerprintBits > 0)
  require(maxNumKicks > 0)
  require(fingerprintsPerBucket > 0)

  private val bucketIndexBits = 32 - JInteger.numberOfLeadingZeros(fingerprintsPerBucket)

  private val bucketBits = (1 << fingerprintBits) * fingerprintsPerBucket + bucketIndexBits

  private val buffer = new Array[Byte]((bucketBits * totalBuckets + 7) / 8)

  def fingerprintsInBucket(bucket: Long): Int = buffer.unsigned(bucketBits * bucket, bucketIndexBits).toInt

  def fingerprintsInBucket(bucket: Long, value: Long): Unit = buffer.update(bucketBits * bucket, bucketIndexBits, value)

  def setFingerprint(bucket: Long, fingerprintIndex: Int, fingerprint: Long): Unit = {
    buffer.update(
      bucketBits * bucket + bucketIndexBits + fingerprintBits * fingerprintIndex, fingerprintBits, fingerprint)
  }

  def getFingerprint(bucket: Long, fingerprintIndex: Int): Long = {
    buffer.unsigned(bucketBits * bucket + bucketIndexBits + fingerprintBits * fingerprintIndex, fingerprintBits)
  }

  def fingerprintIsInBucket(bucket: Long, fingerprint: Long): Boolean = {
    val fingerprints = fingerprintsInBucket(bucket)

    @tailrec def go(index: Int): Boolean = {
      if (index < fingerprints) {
        val f = getFingerprint(bucket, index)

        if (f == fingerprint) {
          true
        } else {
          go(index + 1)
        }
      } else {
        false
      }
    }

    go(0)
  }

  def fingerprint[A: Hashable](f: A): Long = implicitly[Hashable[A]].hash(f)

  def addToBucket(bucket: Long, f: Long): Boolean = {
    val fingerprints = fingerprintsInBucket(bucket)

    if (fingerprints < fingerprintsPerBucket) {
      setFingerprint(bucket, fingerprints, f)
      fingerprintsInBucket(bucket, fingerprints + 1)
      true
    } else {
      false
    }
  }

  def swapRandomBucketEntry(bucket: Long, f: Long): Long = {
    val fingerprints = fingerprintsInBucket(bucket)

    if (fingerprints > 0) {
      val candidateIndex = Random.nextInt(fingerprints)
      val candidate = getFingerprint(bucket, candidateIndex)
      setFingerprint(bucket, candidateIndex, f)
      candidate
    } else {
      f
    }
  }

  final def insert[A: Hashable](value: A)(implicit ev: Hashable[Long]): Boolean = {
    var f = fingerprint(value)
    val i1 = value.hashed
    val i2 = i1 ^ f.hashed

    addToBucket(i1, f) || addToBucket(i2, f) || {
      // must relocate existing items
      var i = if (Random.nextBoolean()) i1 else i2

      for (n <- 0 until maxNumKicks) {
        f = swapRandomBucketEntry(i, f)

        i = i ^ f.hashed

        if (addToBucket(i, f)) {
          return true
        }
      }

      false
    }
  }

  final def lookup[A: Hashable](value: A)(implicit ev: Hashable[Long]): Boolean = {
    val f = fingerprint(value)
    val i1 = value.hashed
    val i2 = i1 ^ f.hashed

    fingerprintIsInBucket(i1, f) || fingerprintIsInBucket(i2, f)
  }

  final def delete[A: Hashable](value: A)(implicit ev0: Hashable[FingerPrint]): Unit = {

  }
}
