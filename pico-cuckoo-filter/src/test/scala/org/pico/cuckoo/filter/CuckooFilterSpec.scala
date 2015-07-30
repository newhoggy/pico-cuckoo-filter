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

  "Can insert exactly `fingerprintsPerBucket * 2` number of fingerprints into two buckets" in {
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
}
