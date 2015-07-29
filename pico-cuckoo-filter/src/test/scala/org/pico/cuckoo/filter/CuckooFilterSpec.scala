package org.pico.cuckoo.filter

import org.pico.hash.Hashable
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.pico.hash.syntax._

import scala.util.hashing.MurmurHash3

class CuckooFilterSpec extends Specification with ScalaCheck {
  implicit val hashableString = new Hashable[String] {
    override def hash(a: String): Long = MurmurHash3.stringHash(a)
  }

  "Get the hash of a string" in {
    prop { (text: String) =>
      println(s"$text => ${text.hashed}")
      success
    }
  }
}
