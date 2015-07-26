package org.pico.twiddle

trait FixedInt2FixedInt[@specialized(Byte, Short, Int, Long) A, @specialized(Byte, Short, Int, Long) B] {
  def fixAs(self: A): B
}
