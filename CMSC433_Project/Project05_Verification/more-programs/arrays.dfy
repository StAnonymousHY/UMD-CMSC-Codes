method ArrayTest(a : array<int>) returns (x : int)
  requires a.Length > 0
  ensures x > 0 
{
    x := a[0];
}