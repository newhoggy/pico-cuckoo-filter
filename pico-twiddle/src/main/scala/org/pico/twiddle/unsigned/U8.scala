package org.pico.twiddle.unsigned

case class U8(value: Byte) extends AnyVal {
  @inline final def +(that: U8): U8 = U8((this.value + that.value).toByte)

  @inline final def -(that: U8): U8 = U8((this.value - that.value).toByte)

  @inline final def to_u8: U8 = this

  @inline final def to_u16: U16 = U16((value.toInt & 0xff).toShort)

  @inline final def to_u32: U32 = U32(value.toInt & 0xff)

  @inline final def to_u64: U64 = U64(value.toLong & 0xffL)

  @inline final def u8: U8 = to_u8

  @inline final def u16: U16 = to_u16

  @inline final def u32: U32 = to_u32

  @inline final def u64: U64 = to_u64

  @inline final def >>(offset: Byte): U8 = (u32 >> offset).to_u8

  @inline final def <<(offset: Byte): U8 = (u32 << offset).to_u8
}
