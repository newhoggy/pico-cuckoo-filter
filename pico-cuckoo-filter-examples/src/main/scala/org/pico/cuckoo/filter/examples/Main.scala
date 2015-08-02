package org.pico.cuckoo.filter.examples

import org.pico.cuckoo.filter._
import org.pico.hash.syntax._
import org.pico.hash.{Hash64, Hashable}
import org.pico.twiddle.syntax.anyVal._
import scala.util.hashing.MurmurHash3

object Main {
  implicit val hashableString = new Hashable[String] {
    override def hash(a: String): Hash64 = Hash64(MurmurHash3.stringHash(a))
  }

  implicit val hashableLong = new Hashable[Long] {
    override def hash(a: Long): Hash64 = Hash64(MurmurHash3.arrayHash(Array(a)))
  }

  implicit val hashableFingerprint = new Hashable[Fingerprint] {
    override def hash(a: Fingerprint): Hash64 = a.value.hashed
  }

  def main(args: Array[String]): Unit = {
    val filter = new CuckooFilter(
      16, // The number fingerprints per bucket
      24.bits, // The number of bits in a finger print
      5, // The maximum number of kicks to attempt before failing an insert
      128) // The total number of buckets

    val a = filter.insert("Element") // Returns true if the insertion was successful
    val b = filter.lookup("Element") // Returns true since the element was just inserted
    val c = filter.delete("Element") // Returns true since the element was still in the filter
    val d = filter.lookup("Element") // Returns false since the element has just been deleted
    val e = filter.delete("Element") // Returns false since the element has already been deleted

    println(s"$a $b $c $d $e")
  }
}