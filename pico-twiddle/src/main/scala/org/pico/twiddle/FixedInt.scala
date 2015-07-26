package org.pico.twiddle

trait FixedInt[@specialized(Byte, Short, Int, Long) A] {
  def bitSize: Int
  def hex(self: A): String

  def ufixed[B](self: A): B
  def ubyte(self: A): Byte
  def ushort(self: A): Short
  def uint(self: A): Int
  def ulong(self: A): Long

  def >>>>(self: A, offset: Long): A
  def <<<<(self: A, offset: Long): A
  def ||||(self: A, that: A): A
}
