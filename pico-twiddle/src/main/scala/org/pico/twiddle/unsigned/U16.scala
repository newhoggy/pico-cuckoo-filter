package org.pico.twiddle.unsigned

case class U16(value: Short) extends AnyVal {
  @inline final def +(that: U16): U16 = U16((this.value + that.value).toShort)

  @inline final def -(that: U16): U16 = U16((this.value - that.value).toShort)

  @inline final def to_u8: U8 = U8(value.toByte)

  @inline final def to_u16: U16 = this

  @inline final def to_u32: U32 = U32(value.toInt & 0xffff)

  @inline final def to_u64: U64 = U64(value.toLong & 0xffffL)

  @inline final def u16: U16 = to_u16

  @inline final def u32: U32 = to_u32

  @inline final def u64: U64 = to_u64

  @inline final def >>(offset: Byte): U16 = (u32 >> offset).to_u16

  @inline final def <<(offset: Byte): U16 = (u32 << offset).to_u16
}
