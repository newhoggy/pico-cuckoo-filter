package org.pico.cuckoo.filter

import java.lang.{Integer => JInteger}

import org.pico.hash.{Hash64, Hashable}
import org.pico.hash.syntax._
import org.pico.twiddle.instances._
import org.pico.twiddle.syntax.arrayIndexed._

import scala.annotation.tailrec
import scala.util.Random

class CuckooFilter(fingerprintsPerBucket: Int, fingerprintBits: Int, maxNumKicks: Int = 5, totalBuckets: Int = 128) {
  require(fingerprintBits > 0)
  require(maxNumKicks > 0)
  require(fingerprintsPerBucket > 0)

  private val bucketIndexBits = 32 - JInteger.numberOfLeadingZeros(fingerprintsPerBucket)

  private val bucketBits = (1 << fingerprintBits) * fingerprintsPerBucket + bucketIndexBits

  private val buffer = new Array[Byte]((bucketBits * totalBuckets + 7) / 8)

  def bucketIndex(hash: Hash64): Long = bucketBits * (hash.value & 0x7fffffffffffffffL) % totalBuckets

  def fingerprintsInBucketForHash(hash: Hash64): Int = {
    println(s"fingerprintsInBucket($hash) => buffer.unsigned(${bucketIndex(hash)}, $bucketIndexBits).toInt")
    buffer.unsigned(bucketIndex(hash), bucketIndexBits).toInt
  }

  def fingerprintsInBucketForHash(hash: Hash64, value: Long): Unit = buffer.update(bucketIndex(hash), bucketIndexBits, value)

  def setFingerprint(hash: Hash64, fingerprintIndex: Int, fingerprint: Fingerprint): Unit = {
    buffer.update(
      bucketIndex(hash) + bucketIndexBits + fingerprintBits * fingerprintIndex, fingerprintBits, fingerprint.value)
  }

  def getFingerprint(hash: Hash64, fingerprintIndex: Int): Fingerprint = {
    Fingerprint(
      buffer.unsigned(bucketIndex(hash) + bucketIndexBits + fingerprintBits * fingerprintIndex, fingerprintBits))
  }

  def removeFingerprintFromBucketForHash(hash: Hash64, f: Fingerprint): Boolean = {
    val fingerprints = fingerprintsInBucketForHash(hash)
    val index = fingerprintIndex(hash, f)

    if (index != -1) {
      setFingerprint(hash, index, getFingerprint(hash, fingerprints - 1))
      setFingerprint(hash, fingerprints - 1, Fingerprint(0))
      true
    } else {
      false
    }
  }

  def fingerprintIndex(bucket: Hash64, fingerprint: Fingerprint): Int = {
    val fingerprints = fingerprintsInBucketForHash(bucket)

    @tailrec def go(index: Int): Int = {
      if (index < fingerprints) {
        val f = getFingerprint(bucket, index)

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

  def fingerprintIsInBucket(hash: Hash64, fingerprint: Fingerprint): Boolean = fingerprintIndex(hash, fingerprint) != -1

  def fingerprint[A: Hashable](a: A): Fingerprint = Fingerprint(implicitly[Hashable[A]].hash(a).value)

  def addToBucket(hash: Hash64, f: Fingerprint): Boolean = {
    val fingerprints = fingerprintsInBucketForHash(hash)

    if (fingerprints < fingerprintsPerBucket) {
      setFingerprint(hash, fingerprints, f)
      fingerprintsInBucketForHash(hash, fingerprints + 1)
      println(s"inserted into bucket ${bucketIndex(hash)}")
      true
    } else {
      println(s"could not insert into bucket ${bucketIndex(hash)}")
      false
    }
  }

  def swapRandomBucketEntry(hash: Hash64, f: Fingerprint): Fingerprint = {
    val fingerprints = fingerprintsInBucketForHash(hash)

    if (fingerprints > 0) {
      val candidateIndex = Random.nextInt(fingerprints)
      val candidate = getFingerprint(hash, candidateIndex)
      setFingerprint(hash, candidateIndex, f)
      candidate
    } else {
      f
    }
  }

  final def insert[A: Hashable](value: A)(implicit ev0: Hashable[Long], ev1: Hashable[Fingerprint]): Boolean = {
    var f = fingerprint(value)
    val i1 = value.hashed
    val i2 = i1 ^ f.hashed

    println(s"insert($value)")

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

  final def lookup[A: Hashable](value: A)(implicit ev0: Hashable[Long], ev1: Hashable[Fingerprint]): Boolean = {
    val f = fingerprint(value)
    val i1 = value.hashed
    val i2 = i1 ^ f.hashed

    fingerprintIsInBucket(i1, f) || fingerprintIsInBucket(i2, f)
  }

  final def delete[A: Hashable](value: A)(implicit ev0: Hashable[Long], ev1: Hashable[Fingerprint]): Boolean = {
    val f = fingerprint(value)
    val i1 = value.hashed
    val i2 = i1 ^ f.hashed

    removeFingerprintFromBucketForHash(i1, f) || removeFingerprintFromBucketForHash(i2, f)
  }
}
