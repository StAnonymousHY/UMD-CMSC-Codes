method LinearSearch(a: array<int>, key: int) returns (index: int)
  ensures 0 <= index < a.Length ==> a[index] == key
  ensures index < 0 ==> forall k : int :: 0 <= k < a.Length ==> a[k] != key
{
  index := -1;
  var i : int := 0;
  while i < a.Length
    invariant i <= a.Length && (index == -1 ==> forall k : int :: 0 <= k < i ==> a[k] != key) && (index >= 0 ==> index < a.Length && a[index] == key)
  {
    if a[i] == key 
    { 
      index := i;
    }
    i := i + 1;
  }
}