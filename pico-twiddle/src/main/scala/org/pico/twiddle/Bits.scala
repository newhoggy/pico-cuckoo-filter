package org.pico.twiddle

import org.pico.twiddle.syntax.fixedInt._

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

  def min(that: Bits): Bits = Bits(this.value min that.value)
  def max(that: Bits): Bits = Bits(this.value max that.value)

  def *(that: Long)(implicit ev: DummyImplicit): Bits = Bits(this.value * that)
  def /(that: Long)(implicit ev: DummyImplicit): Bits = Bits(this.value / that)
  def %(that: Long)(implicit ev: DummyImplicit): Bits = Bits(this.value % that)

  def /+(that: Bits): Int = ((this + that - Bits(1)) / that).value.toInt

  def until(that: Bits) = (this.value until that.value).map(Bits)
}
