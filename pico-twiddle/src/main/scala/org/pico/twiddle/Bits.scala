package org.pico.twiddle

case class Bits(value: Long) extends AnyVal {
  def +(that: Bits): Bits = Bits(this.value + that.value)
  def -(that: Bits): Bits = Bits(this.value - that.value)
  def *(that: Bits): Bits = Bits(this.value * that.value)
  def /(that: Bits): Bits = Bits(this.value / that.value)
  def %(that: Bits): Bits = Bits(this.value % that.value)

  def <(that: Bits): Boolean = this.value < that.value
  def >(that: Bits): Boolean = this.value > that.value
  def <=(that: Bits): Boolean = this.value <= that.value
  def >=(that: Bits): Boolean = this.value >= that.value
}
