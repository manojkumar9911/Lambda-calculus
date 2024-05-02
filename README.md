# Lambda-calculus

My notes on Lambda Calculus.

Introduction
The syntax of the lambda-calculus comprises just three sorts of terms.

A variable x by itself is a term;
The abstraction of a variable x from a term t1, written λx.t1, is a term;
And the application of a term t1 to another term t2, written t1 t2, is a term.
These ways of forming terms are summarized in the following grammar.

Grammar
t ::=      -- Terms
  x        -- Variable
  λx. t    -- Abstraction
  t t      -- Application
Substitution
[x → s]x       = s
[x → s]y       = y                          -- if y ≠ x
[x → s](λy.t1) = λy.[x → s]t1               -- if y ≠ x and y ∉ FV(s)
[x → s](t1 t2) = ([x → s]t1 ([x → s]t2)
Boolean Definitions
'''
true = λx. λy. x;
false = λx. λy. y;
true a b = a
false a b = b
'''

tests
test = λl. λm. λn. l m n;

test pred l m = pred l m
test tru "a" "b" -- "a"
test fls "a" "b" -- "b"
Reducing Test term
 test tru v w
 = (λl.λm.λn.l m n) tru v w         by definition
 → (λm.λn.tru m n) v w
 → (λn.tru v n) w
 → tru v w
 = (λt.λf.t) v w                    by definition
 → (λf. v) w
 → v
Logical Operators
# AND
'''
and = λa. λb. a b false;
'''

# OR
'''
or = λa. λb. a true b;
'''

# XOR
'''
 xor = λa. λb. a (not b) b;
'''

# NOT
''' not = λa. a false true;
'''

# Pairs
pair = λf.λs.λb. b f s;
fst = λp. p tru;
snd = λp. p fls;
Pair v w is a function that, when applied to a boolean value b, applies b to v and w.

pair f s b = b f s
fst p = p tru
snd p = p fls

Reducing fst
  fst (pair v w)
= fst ((λf.λs.λb.b f s) v w)
→ fst ((λs.λb.b v s) w)
→ fst (λb. b v w)
= (λp.p tru) (λb.b v w)
→ (λb.b v w) tru
→ tru v w
→ v

Church Numerals
For representing numbers by lambda-terms
A number n is represented by a combinator (one, two, three, etc. below) that takes two arguments, s and z, and applies s, n times, to z.
As with booleans and pairs, this encoding makes numbers into active entities: the number n is represented by a function that does something n times — a kind of active unary numeral.
zero  = λs. λz. z;            -- applies s to z zero times
one   = λs. λz. s z;          -- applies s to z once
two   = λs. λz. s (s z);      -- applies s twice
three = λs. λz. s (s (s z));  -- applies s three times
etc.                          -- and so on...

# Successor
Remember, a Church numeral n is a function that applies s to z (n times).

scc = λn. λs. λz. s (n s z); -- Successor function
The term scc is a combinator that takes a Church numeral n and returns another Church numeral—that is, it yields a function that takes arguments s and z and applies s repeatedly to z. We get the right number of applications of s to z by first passing s and z as arguments to n, and then explicitly applying s one more time to the result.

scc  n s z = s (n s z)
scc' n s = s . n s
We can think of successor function in two ways. One way, as defined above, is applying s one more time to given number. Another way is to add given number n to one. Second approach is given below:

scc = λn. λs. λz. n s (s z)
scc  n s z = n s (s z)
Let us see if scc one is actually two:

scc one
= scc (λs. λz. s z)                       -- by definition of one
= (λn. λs. λz. n s (s z)) (λs. λz. s z)   -- by definition of scc
→ (λs. λz. (λs. λz. s z) s (s z))
→ λs. λz. (λz. s z)(s z)
→ λs. λz. s (s z)
= two                                     -- by definition, s applied twice

# Addition
Addition of Church numerals can be performed by a term plus that takes two Church numerals, m and n, as arguments, and yields another Church numeral—i.e., a function—that accepts arguments s and z, applies s iterated n times to z (by passing s and z as arguments to n), and then applies s iterated m more times to the result:
'''
plus = λm. λn. λs. λz. m s (n s z);
'''

We can also think of addition in terms of successor (or increment) function.

plus m n = m incr n
Here, we apply m times the incrementation to n. In other words, increment n, m times.

Now, we can write -

two   = plus one one
three = plus two one
four  = add two two

# Multiplication
Multiplying n and m is adding together m copies of n. Notice that, plus m adds m to any given number. If we apply add m n times to zero, we will have added n copies of m.
'''
mul = λm. λn. m (plus n) zero;
'''

Intuitively, having a s.s.s. ... s of length m, in order to multiply it by n, we should combine n copies of such a chain. Or, if (m s) is a "super-successor" containing m exemplars of s, what we need is

mul n m s z = n (m s) z
which is same as

mul n m = n . m
Multiplication is functional composition!

Exponentiation
pow n m means n raised to m-th power. Or, n * n *.....* n- multiplying n by itself m times.

pow n m
 = (mul n ( ... (mul n (mul n one))))  -- m times
 = m (mul n) one
We know that in the theory of sets, for any sets A and B, the notation B^A denotes the set of all functions from A to B. Typically one applies the argument based on cardinality. Adding one element to A, permits to find |B| more mappings, from the additional element to all of B. So, the number of mappings is multiplied by |B|, in agreement with : B^A+1 = B^A * B.

pow n m = m n
exponentiation = inverse application.

isZero
To test whether a Church numeral is zero, we apply our numeral to a pair of terms zz and ss such that applying ss to zz one or more times yields fls, while not applying it at all yields tru.

Remember zero is a function that applies s to z zero times - zero = λs. λz. z or zero  = λs. id.

That is, we need ss and zz such that zero ss zz is tru and fls for any other numeeral.

iszro = λm. m (λx. fls) tru;
Predecessor
Predecessor function is tricky! We begin with zero, and keep counting until n, but storing the previous number. Then when we get n, the previous one is its predecessor. We use Pairs, defined earlier, to keep two previous numbers.

zz = pair zero zero
ss  = λp. pair (snd p) (plus one (snd p));
prd = λm. fst (m ss zz);

# SUBTRACTION:


'''
subtract = λm. λn. m (λf. λx. n f (f x))
sub = λm.λn.n pred m
'''

# DIVISION:

'''
div = λm.λn.if (isZero n) then (error "Division by zero") else (λf.λx.if (isZero m) then zero else succ (div (pred m) n) f x) 
'''

Division is little bit complex because we can't divide by 0,we have to return error in this case.
