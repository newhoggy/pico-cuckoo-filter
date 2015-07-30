package org.pico.hash

case class Hash64(value: Long) extends AnyVal {
  def ^(that: Hash64): Hash64 = Hash64(this.value ^ that.value)
}
