package org.pico.hash.index

package object syntax {
  implicit class HashIndexedOps[A](val self: A) extends AnyVal {
    def hashIndex(implicit ev: HashIndexed[A]): Int = ev.hashIndexOf(self)
  }
}
