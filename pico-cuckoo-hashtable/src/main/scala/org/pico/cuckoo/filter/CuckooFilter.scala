package org.pico.cuckoo.filter

import org.pico.hash.Hashable

class CuckooFilter(bucketSize: Int, fingerPrintBits: Int) {
  require(fingerPrintBits > 0)

  val buffer = new Array[Byte]((1 << fingerPrintBits) * bucketSize)

  final def insert[A: Hashable](value: A)(implicit ev0: Hashable[FingerPrint]): Unit = {

/*
f = fingerprint(x);
i1 = hash(x);
i2 = i1 ⊕ hash(f);
if bucket[i1] or bucket[i2] has an empty entry then
add f to that bucket;
return Done;
// must relocate existing items;
i = randomly pick i1 or i2;
for n = 0; n < MaxNumKicks; n++ do
randomly select an entry e from bucket[i];
swap f and the fingerprint stored in entry e;
i = i ⊕ hash(f);
if bucket[i] has an empty entry then
add f to bucket[i];
return Done;
// Hashtable is considered full;
return Failure;
 */
  }

  final def delete[A: Hashable](value: A)(implicit ev0: Hashable[FingerPrint]): Unit = {

  }
}

object CuckooFilter {

}
