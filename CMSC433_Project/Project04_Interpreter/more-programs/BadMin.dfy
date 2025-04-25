method BadMin(a:int, b:int) returns (z : int) 
  requires a > 0
  requires b > 0 
  ensures z == a || z == b
  ensures z <= a && z <= b
{
      var x : int := a;
      var y : int := b;
      z := 0;
      while x > 0 && y > 0 
        invariant z + x == a && z + y == b
      {
        x := x - 1;             
        y := y - 1;
        z := z + 1;
      }
}