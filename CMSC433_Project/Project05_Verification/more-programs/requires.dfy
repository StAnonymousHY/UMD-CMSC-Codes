method Req(x:int) returns (y:int)
  requires x > 0 
  ensures y > 0 
{
  y := x;
}