method ManyBinops (x : int, y : int, a : bool, b : bool) returns (c : int)
  requires y != 0
{
  if (a && b) || (!a && x < y) || x >= y {
    c := x + y - x / y;
  }
  else {
    c := x * y % y;
  }
}