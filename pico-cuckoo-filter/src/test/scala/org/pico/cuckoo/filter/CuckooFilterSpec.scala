package org.pico.cuckoo.filter

import org.pico.hash.Hashable
import org.scalacheck.Arbitrary.arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.util.hashing.MurmurHash3

class CuckooFilterSpec extends Specification with ScalaCheck {
  implicit val hashableString = new Hashable[String] {
    override def hash(a: String): Long = MurmurHash3.stringHash(a)
  }

  implicit val hashableLong = new Hashable[Long] {
    override def hash(a: Long): Long = MurmurHash3.arrayHash(Array(a))
  }

  "Can insert exactly `fingerprintsPerBucket` number of fingerprints into a bucket" in {
    val filter = new CuckooFilter(
      fingerprintsPerBucket = 16,
      fingerprintBits = 8,
      maxNumKicks = 5,
      totalBuckets = 1)

    for (i <- 0 until 16) {
      val text = arbitrary[String].sample.get

      filter.insert(text) ==== true
    }

    val text = arbitrary[String].sample.get

    filter.insert(text) ==== false
  }
}
