/** Now, time for a larger example. Let's revisit our search example
from last lecture, but now let's require that the input array is sorted.
That enables binary search for the key. 

First, how do we specify sortedness? 

// IN CLASS

What should binary search return? It should return the smallest index n in the 
array, such that all elements before n are strictly smaller, and all elements 
after (and including) n are greater or equal.

// IN CLASS

*/

method BinarySearch(a: array<int>, key: int) returns (n: int)
    requires forall i,j :: 0 <= i < j < a.Length ==> a[i] <= a[j]
    ensures 0 <= n <= a.Length
    ensures forall i :: 0 <= i < n <= a.Length ==> a[i] < key   //<= a.length原因：a.length长度是1，key>a[0] 此时lo=n=1=a.length
    ensures forall i :: 0 <= n <= i < a.Length ==> key <= a[i]
{
  var lo, hi := 0, a.Length;
  while lo < hi
    invariant 0 <= lo <= hi <= a.Length
    invariant forall i :: -9999 <= 0 <= i < lo <= a.Length ==> a[i] < key
    invariant forall i :: hi <= i < a.Length ==> key <= a[i]
  {
    var mid := (lo + hi) / 2;
    if a[mid] < key {
      lo := mid + 1;
    } else {
      hi := mid;
    }
  }
  n := lo;
}

/* As for invariants, what do we know at every point? We start with 
lo at 0 and hi at a.Length - bounds to the array for which one part 
of the postcondition holds trivially. And we just keep shortening 
the range until lo and hi coincide! */

// Now we can wrap the call to BinarySearch inside a convenient Contains function. 

method Contains(a: array<int>, key: int) returns (present: bool)
  requires forall i, j :: 0 <= i < j < a.Length ==> a[i] <= a[j]
  ensures present == exists i :: 0 <= i < a.Length && key == a[i]
{
  var n := BinarySearch(a, key);
  present := n < a.Length && a[n] == key;
}