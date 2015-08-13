package org.pico.hash

package object syntax {
  implicit final class HashableOps_rXvzK6r[A](val self: A) extends AnyVal {
    def hashed(implicit ev: Hashable[A]): Hash64 = ev.hash(self)
  }

  implicit final class Hashable2Ops_rXvzK6r[A](val self: A) extends AnyVal {
    def hashed2(implicit ev: Hashable2[A]): Hash64 = ev.hash2(self)
  }
}
