method Min (x : int, y : int) returns (min : int)
    ensures min <= x && min <= y 
{
    if (x < y) {
        min := x;
    }
    else {
        min := y;
    }
}
