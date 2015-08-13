package org.pico.hash

trait Hashable2[A] {
  def hash2(a: A): Hash64
}
