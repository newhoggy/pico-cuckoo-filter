package org.pico.cuckoo.filter

import org.pico.hash.syntax._
import org.pico.hash.{Hash64, Hashable}
import org.pico.twiddle.Bits
import org.pico.twiddle.syntax.anyVal._
import org.scalacheck.Arbitrary.arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.util.hashing.MurmurHash3

class CuckooFilterSpec extends Specification with ScalaCheck {
  sequential

  implicit val hashableString = new Hashable[String] {
    override def hash(a: String): Hash64 = Hash64(MurmurHash3.stringHash(a))
  }

  implicit val hashableLong = new Hashable[Long] {
    override def hash(a: Long): Hash64 = Hash64(MurmurHash3.arrayHash(Array(a)))
  }

  implicit val hashableFingerprint = new Hashable[Fingerprint] {
    override def hash(a: Fingerprint): Hash64 = a.value.hashed
  }

  "Can insert exactly `fingerprintsPerBucket` number of fingerprints into a bucket" in {
    val filter = new CuckooFilter(fingerprintsPerBucket = 16, fingerprintBits = Bits(8), maxNumKicks = 5, totalBuckets = 1)

    for (i <- 0 until 16) {
      val text = arbitrary[String].sample.get

      filter.insert(text) ==== true
    }

    val text = arbitrary[String].sample.get

    filter.insert(text) ==== false
  }

  "Can insert exactly `fingerprintsPerBucket * 2` number of fingerprints into two buckets of 16 fingerprints each" in {
    val filter = new CuckooFilter(fingerprintsPerBucket = 16, fingerprintBits = 8.bits, maxNumKicks = 5, totalBuckets = 2)

    var inserted = 0

    for (i <- 0 until 64) {
      val text = arbitrary[String].sample.get

      if (filter.insert(text)) {
        inserted += 1
      }
    }

    inserted ==== 32
  }

  "Inserted value can be looked up" in {
    prop { text: String =>
      val filter = new CuckooFilter(fingerprintsPerBucket = 16, fingerprintBits = 24.bits, maxNumKicks = 5, totalBuckets = 2)

      filter.insert(text)

      filter.lookup(text) === true
    }
  }

  "Inserted elements can be looked up whilst elements not inserted cannot" in {
    val filter = new CuckooFilter(fingerprintsPerBucket = 16, fingerprintBits = 24.bits, maxNumKicks = 5, totalBuckets = 16)
    val inclusions = arbitrary[Set[String]].sample.get
    val exclusions = arbitrary[Set[String]].sample.get -- inclusions

    for (i <- inclusions) {
      if (!filter.insert(i)) {
        failure("An insert was rejected")
      }
    }

    for (i <- inclusions) {
      filter.lookup(i) ==== true
    }

    for (e <- exclusions) {
      filter.lookup(e) ==== false
    }

    ok
  }

  "Inserted elements can be looked up whilst deleted elements cannot" in {
    val filter = new CuckooFilter(fingerprintsPerBucket = 16, fingerprintBits = 24.bits, maxNumKicks = 5, totalBuckets = 16)
    val deletions = arbitrary[Set[String]].sample.get
    val remaining = arbitrary[Set[String]].sample.get -- deletions
    val insertions = deletions ++ remaining

    for (i <- insertions) {
      if (!filter.insert(i)) {
        failure("An insert was rejected")
      }
    }

    for (i <- deletions) {
      if (!filter.delete(i)) {
        failure("A delete was rejected")
      }
    }

    for (i <- remaining) {
      filter.lookup(i) ==== true
    }

    for (e <- deletions) {
      filter.lookup(e) ==== false
    }

    ok
  }
}
