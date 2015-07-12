package org.pico.collection

import org.pico.hash.index.HashIndexed
import org.pico.hash.index.syntax._

sealed trait DumbHashMap[-A, +B] {
  def size: Long = 0

  def get[AA <: A](key: A)(implicit ev: HashIndexed[AA]): Option[B]
}

object DumbHashMap {
  val levelBits = 8
  val bucketSize = 8

  def empty[A] = EmptDumbyHashMap
}

case class LeafDumbHashMap[A, B](table: Array[B], override val size: Int) extends DumbHashMap[A, B] {
  override def get[AA <: A](key: A)(implicit ev: HashIndexed[AA]): Option[B] = {
    assert(ev.hashIndexBits == DumbHashMap.levelBits)
    Option(table(key.hashIndex))
  }
}

case object EmptDumbyHashMap extends DumbHashMap[Any, Nothing] {
  override def size: Long = 0

  override def get[AA <: Any](key: Any)(implicit ev: HashIndexed[AA]): Option[Nothing] = None
}
