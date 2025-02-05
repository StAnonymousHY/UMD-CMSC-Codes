method IsPrime (m : int) returns (isPrime : bool)
    requires m > 0
    ensures isPrime <==> (m > 1 && forall j : int :: 2 <= j < m ==> m % j != 0)
{
    isPrime := true;
    var i := 2;
    if(m == 1)
    {
        return false;
    }
    while(i < m)
        invariant i <= m
        invariant isPrime <==> (m > 1 && forall j : int :: 2 <= j < i <= m ==> m % j != 0)
    {
        if (m % i == 0)
        {
            return false;
        }
        i:=i+1;
    }
}