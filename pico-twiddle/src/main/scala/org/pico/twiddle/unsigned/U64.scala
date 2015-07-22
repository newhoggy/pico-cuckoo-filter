package org.pico.twiddle.unsigned

case class U64(value: Long) extends AnyVal {
  @inline final def +(that: U64): U64 = U64(this.value + that.value)

  @inline final def -(that: U64): U64 = U64(this.value - that.value)

  @inline final def to_u8: U8 = U8(value.toByte)

  @inline final def to_u16: U16 = U16(value.toShort)

  @inline final def to_u32: U32 = U32(value.toInt)

  @inline final def to_u64: U64 = this

  @inline final def u64: U64 = to_u64

  @inline final def >>>>(offset: Long): U64 = {
    offset match {
      case o if o >= 64 => U64(0L)
      case o if o > 0   => U64(value >>> offset)
      case o if o > -64 => U64(value << -offset)
      case _            => U64(0L)
    }
  }

  def <<<<(offset: Long): U64 = {
    offset match {
      case o if o >= 64 => U64(0L)
      case o if o > 0   => U64(value << offset)
      case o if o > -64 => U64(value >>> -offset)
      case _            => U64(0L)
    }
  }
}
