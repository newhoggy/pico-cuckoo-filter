package org.pico.cuckoo.filter.examples

import java.lang.{Long => JLong}

import org.pico.cuckoo.filter._
import org.pico.hash.syntax._
import org.pico.hash.{Hash64, Hashable, Hashable2}
import org.pico.twiddle.syntax.anyVal._

import scala.util.Random
import scala.util.hashing.MurmurHash3

object Main {
  implicit val hashableString = new Hashable[String] {
    override def hash(a: String): Hash64 = Hash64(MurmurHash3.stringHash(a))
  }

  implicit val hashable2String = new Hashable2[String] {
    override def hash2(a: String): Hash64 = Hash64(JLong.reverse(MurmurHash3.stringHash(a)))
  }

  implicit val hashableLong = new Hashable[Long] {
    override def hash(a: Long): Hash64 = Hash64(a)
  }

  implicit val hashable2Long = new Hashable2[Long] {
    override def hash2(a: Long): Hash64 = Hash64(JLong.reverse(MurmurHash3.arrayHash(Array(a))))
  }

  implicit val hashableFingerprint = new Hashable[Fingerprint] {
    override def hash(a: Fingerprint): Hash64 = a.value.hashed
  }

  def main(args: Array[String]): Unit = {
    val filter = new CuckooFilter(
      16, // The number fingerprints per bucket
      24.bits, // The number of bits in a finger print
      5, // The maximum number of kicks to attempt before failing an insert
      10000) // The total number of buckets

    var x = 0

    while (x < 100000) {
      filter.insert(Random.nextLong())

      if (x % 1000 == 0) {
        println(s"$x")
      }

      x += 1
    }
  }
}