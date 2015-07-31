package org.pico.cuckoo.filter

import org.pico.twiddle.Bits
import org.pico.twiddle.instances._
import org.pico.twiddle.syntax.fixedInt._

case class Fingerprint(value: Long) extends AnyVal

object Fingerprint {
  def apply(value: Long, bits: Bits): Fingerprint = {
    val zeroBits = (bitSize[Long] - bits)
    Fingerprint(value <<<< zeroBits >>>> zeroBits)
  }
}
