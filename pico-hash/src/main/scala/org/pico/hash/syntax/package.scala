package org.pico.hash

package object syntax {
  implicit final class HashableOps[A](val self: A) extends AnyVal {
    def hashed(implicit ev: Hashable[A]): Long = ev.hash(self)
  }
}
