method Max (x : int, y : int) returns (max : int)
    ensures max == x || max == y
{
    if (x < y) {
        max := y;
    }
    else {
        max := x;
    }
}
