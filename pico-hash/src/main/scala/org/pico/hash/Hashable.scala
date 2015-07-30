package org.pico.hash

trait Hashable[A] {
  def hash(a: A): Hash64
}
