package org.pico.twiddle.unsigned

case class U32(value: Int) extends AnyVal {
  @inline final def +(that: U32): U32 = U32(this.value + that.value)

  @inline final def -(that: U32): U32 = U32(this.value - that.value)

  @inline final def to_u8: U8 = U8(value.toByte)

  @inline final def to_u16: U16 = U16(value.toShort)

  @inline final def to_u32: U32 = this

  @inline final def to_u64: U64 = U64(value.toLong & 0xffffL)

  @inline final def u32: U32 = to_u32

  @inline final def u64: U64 = to_u64

  @inline final def >>(offset: Long): U32 = {
    offset match {
      case o if o >= 32 => U32(0)
      case o if o > 0   => U32(value >>> offset)
      case o if o > -32 => U32(value << -offset)
      case _            => U32(0)
    }
  }

  @inline final def <<(offset: Long): U32 = {
    offset match {
      case o if o >= 32 => U32(0)
      case o if o > 0   => U32(value << offset)
      case o if o > -32 => U32(value >>> -offset)
      case _            => U32(0)
    }
  }
}
