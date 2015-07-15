package org.pico.collection.mutable

import org.pico.hash.Hashable

class CuckooHashMap[A](capacity: Int, layout: CuckooHashSetEntryLayout[A]) {
//  private var buffer: Array[Byte] = new Array[Byte](layout.endIndex(0) / 8 + 1)



  def +=(value: A)(implicit hashable: Hashable[A]): Unit = {
    ???
  }
}

object CuckooHashMap {
  def empty[A: CuckooHashSetEntryLayout] = new CuckooHashMap[A](128, implicitly[CuckooHashSetEntryLayout[A]])
}
