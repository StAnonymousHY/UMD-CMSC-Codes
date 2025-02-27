/* A First Taste of Dafny

This is the first of many lecture note files that we will go over in
class and that will be made available right after.

The first topic is going to be Dafny itself, what it is and what it
brings to the table. Lecture notes will be compileable files in 
the programming language we are looking at (Dafny/Haskell) that 
you can edit or play around with at your leisure.

*/

/* Dafny: A Standard Imperative Programming Language */

/* At a first glance, Dafny programs can be viewed as standard 
imperative programs. While there are minor syntactic differences,
from keywords to assignment operators and so on, you are probably
able to _read_ plain Dafny code already. Let's try it!

What does the following function do?

*/

method Mystery(a: array<int>, x: int) returns (i: int)
{
    i := 0;
    while i < a.Length
    {
        if a[i] == x
        { 
            return; //要是没有写parameter，那就默认是method开头的returns后面定义的parameter当前的值
        }
        i := i + 1;
    }
    i := -1;
}
method Main() 
{
    //Both explicit and implicit typing
    var i : int := 1;
    var j := 2;
    
}

/** Dafny: A Verification Aware Programming Language */

/* Dafny offers a way to _prove_, mathematically, once and for all that 
your program does what it's supposed to do. But how does it know what 
it's supposed to do? That is the role of a _specification_. Every method 
can be annotated logical expressions that express what the method expects 
from its arguments, and what it guarantees about its output.

We will return to the LinearSearch example shortly, but let's start
by trying to verify an even simpler program: accessing an array element.\

The following method simply returns the i-th element of an array. 
*/
method Access(a:array<int>, i:int) returns (x:int) requires 0 <= i < a.Length ensures 3*2==6 //requires保证precondition，ensures保证postcondition
{
    if 0 <= i < a.Length {
        return a[i];
    }
    return -1;
}

