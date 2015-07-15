package org.pico.collection.mutable

trait Layout[A] {
  def start(parent: Layout[A], index: Int): Int

  def size(parent: Layout[A], index: Int): Int

  def end(parent: Layout[A], index: Int): Int
}

trait BucketLayout

trait CuckooHashSetEntryLayout[A] {
  def presence: Layout[Nothing]

  def buckets: Layout[BucketLayout]

  def bucket: Layout[Nothing]
}


object Moo {
  implicit val c: CuckooHashSetEntryLayout[Int] = ???
  implicit val n: Layout[Nothing] = ???

  c.presence.size(n, 0)
//  c.buckets.size(c.bucket, 0)
}