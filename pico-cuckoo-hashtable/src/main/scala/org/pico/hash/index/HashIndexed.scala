package org.pico.hash.index

sealed trait HashIndexed[A] {
  def hashIndexOf(value: A): Int

  def hashIndexBits: Int
}
