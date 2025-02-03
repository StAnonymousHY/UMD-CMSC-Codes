/** Verification with Dafny */

/* Verification in Dafny is all about specifications. 
   In this lecture, our goal will be to specify and verify
   a sequence of increasingly more complex functions, to 
   get a feeling about what we can/can't prove automatically,
   as well as a first taste of coming up with invariants. 
   
   Meanwhile, we will be introducing more syntax and intricacies
   of the Dafny language.
   */

/* To begin with, consider the following `Min` function. It 
   takes two integers, a and b, and returns two integers, x and y.
   This is the first, perhaps surprising, feature of Dafny: methods
   can have multiple outputs! This goes hand in hand with the slightly
   unconvential way that Dafny handles method returns---the return
   value is whatever is assigned to a return variable at the end of 
   the control flow. But that can easily scale to multiple outputs! */
method Min(a : int, b: int) returns (x : int, y : int)
  ensures x <= y
  ensures (x == a && y == b) || (x == b && y == a)
{
  if a <= b
  {
    x := a;
    y := b;
  }
  else
  {
    x := b;
    y := a;
  }
}

/* As it stands, Dafny accepts this definition. There is no specification 
to check, no array accesses, nothing that can really go wrong. Or is there?
Try deleting an assignment or a branch: Dafny will immediately complain that
an "out-parameter which is subject to definite-assignment rules might be uninitialized".
What that means is that, by default, Dafny will check that every output parameter
(and every variable that is used) will be initialized.

But we haven't actually specified what it means for `Min` to actually compute
the minimum of two numbers. What are possible specifications? 
 */

/* Let's do another loop-free example: evaluating 2-nd degree polynomials:
How do we evaluate something like a*x^2 + b*x + c? We could just evaluate
this expression - but that's inefficient: we perform too many multiplications!
Here's the standard more efficient way:
*/
method Poly2(a:int,b:int,c:int,x:int) returns (out:int)
  ensures out == a*x*x+b*x+c
{
  out := a*x*x+b*x+c;
}
/* For most straight-line code, Dafny can prove the correctness of your 
code with respect to its specification automatically. But there is a 
limit to what we can program without any kind of loop or recursion. */

/** Dafny verification and Loops */

/* Let's start with the simplest loop possible: */
method LoopToZero(n : nat) returns (x : nat)
  ensures x == 0
{
  x := n;
  while x > 0
  {
    x := x - 1;
  }
}

/* Let's start with a simple loop: */
method SquareRoot(N: int) returns (r: int)
  requires N >= 0
  ensures r*r <= N < (r+1)*(r+1)
{
  r := 0;
  while (r + 1) * (r + 1) <= N
    invariant r*r <= N
  {
    r := r + 1;
  }
}

/* A nice feature of verification is that we can verify more complicated
and more efficient ways of performing the same computation. Why does
the following function verify? */

method EfficientSquareRoot(N: int) returns (r: int)
  requires N >= 0
  ensures r * r <= N < (r + 1) * (r + 1)
{
  r := 0;
  var s := 1;
  while s <= N
    invariant r * r <= N
    invariant s == (r+1)*(r+1)
  {
    s := s + 2*r + 3;
    r := r + 1;
  }
}




/* Sticking to the "minimum" theme, let's look at a method for 
   finding the minimum element of an array. We start by initializing
   the return value to the first element of the array; then, we iterate
   through the rest of the array updating the result when appropriate.

  1. Array access
  2. Spec: the result m should be less-than-or-equal to all elements
  3. Invariant?
   */
method FindMin (a : array<int>) returns (m : int)
  requires a.Length > 0
  ensures forall j :: 0 <= j < a.Length ==> m <= a[j]
{
  m := a[0];
  var i:= 1;
  while (i < a.Length)
    invariant i <= a.Length
    invariant forall k :: 0 <= k < i ==> m <= a[k]  //保证到当前的i的时候m是最小的
  {
    if a[i] < m
    { m := a[i]; }
    i := i+1;
  }
}
/** Functional specifications in Dafny.

As discussed in the introduction, Dafny consists of a 
language of expressions which is extended via commands to a language
for implementing imperative methods, and via quantifiers to a 
language for specifications. Dafny includes another layer: a full functional 
language. Let's see that in practice with our favorite example: Fibonnacci numbers.
*/

function fib(n:int) : int {
  if n < 2 then n else fib(n - 2) + fib(n - 1)
}

/* The syntax for functions in Dafny is straightforward: we declare
them with the `function` keyword, followed by the name of the function,
any arguments, and the return type of the expression. The body of the 
function consists of a single expression - what the function is expected 
to return. 

What can we do with this definition? For the most part, we will be using
such functions as specifications:
*/
method Fib(n:int) returns (x : int)
  requires n >= 0
  ensures x == fib(n)
{
  x := 0;
  var y := 1;
  var i := 0;
  while i != n  //Dafny会确保loop必须终止
    invariant x == fib(i)
    invariant y == fib(i+1)
    invariant i <= n
  {
    x, y := y, x + y; // NEW SYNTAX: Simultaneous assignment!
    i := i + 1;
  }
}
/* That is, the imperative, efficient implementation that mutates
two variables in place to keep track of the last two values of 
the Fibonacci sequence, correctly implements the trivial but 
inefficient functional implementation. */

/* For another example, let's implement a functional and imperative version
of the factorial function, and verify that they correspond! */
function fact (n : int) : int
{
  if (n <= 0) then 1 else n * fact(n-1)
}

/* A note on termination:

By default, Dafny checks that any recursive function terminates using
heuristics. In particular, try changing the `n <= 0` check to an equality.
Dafny will complain that "decreases must be bounded below by 0". Termination
checking works by finding some quantity that decreases at every iteration
while being bounded from below. In this particular case, Dafny would correctly
complain that if the integer n is negative, fact would loop forever.
*/

method Fact(n : int) returns (x : int)
  requires n >= 0
  ensures x == fact(n)
{
  var i := n; // NOTE: n is not a variable that can be assigned to!
  x := 1;
  while (i > 0)
    invariant x*fact(i) == fact(n)  //尽量不要用除法，容易触发divisionByZero
  {
    x := x * i;
    i := i - 1;
  }
}

/* Test yourself! Write an imperative version of factorial that counts
up to n rather than down to 0! */

method FactCountingUp(n : int) returns (x : int)
  requires n >= 0
  ensures x == fact(n)
{
  var i := 1;
  x := 1;
  while (i <= n)
    //invariant可以理解为loop的ensures 第一个statement保证while结束后x==fact(i-1) 第二个statement保证while结束后i=n+1
    //（猜想：要是while里面i--的话 那就是保证while结束后i=1 反之才是i=n+1）
    invariant x == fact(i-1) && 0 < i <= n+1
  {
    x := x * i;
    i := i + 1;
  }
}

method FactCountingUpAlternative(n : int) returns (x : int)
  requires n >= 0
  ensures x == fact(n)
{
  var i := 0;
  x := 1;
  while (i <= n-1)
    invariant x == fact(i)
    invariant 0 <= i <= n
  {
    x := x * (i+1);
    i := i + 1;
  }
}

method FactCountingUpAlternativeAlternative(n : int) returns (x : int)
  requires n >= 0
  ensures x == fact(n)
{
  var i := 1;
  x := 1;
  while (i <= n-1)
    //invariant (n == 0 && i == 1 && x == fact(0)) || (n > 0 && 0 <= i <= n && x == fact(i))
    //invariant (n == 0 && i == 1 && x == fact(1)) || (n > 0 && 0 <= i <= n && x == fact(i))
    //invariant (n == 0 && i == 1 && x == fact(i)) || (n > 0 && 0 <= i <= n && x == fact(i))
    //invariant (n == 0 && i == 1 && x == fact(i-1)) || (n > 0 && 0 <= i <= n && x == fact(i))
    //invariant (n == 0 && i == 1 && x == 1) || (n > 0 && 0 <= i <= n && x == fact(i))
    invariant (n == 0 && i > n && x == fact(0)) || (n > 0 && 0 <= i <= n && x == fact(i))
  {
    x := x * (i+1);
    i := i + 1;
  }
}