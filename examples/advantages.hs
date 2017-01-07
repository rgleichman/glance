{-Advantages of Glance:

The following are several admittedly contrived examples demonstrating some
of the advantages of Glance over textual code.


1. Display out of order arguments:

The function "syntaxGraphToTuple" is a simple function that converts the
product type "SyntaxGraph" to a tuple.

syntaxGraphToTuple (SyntaxGraph a b c d e) = (a, b, c, d, e)
-}
syntaxGraphToTuple (SyntaxGraph a b c d e) = (a, b, c, d, e)

{-
Below is a version of syntaxGraphToTuple where some of the items in the tuple have
been swapped.

badSyntaxGraphToTuple (SyntaxGraph a b c d e) = (e, b, c, d, a)
-}
badSyntaxGraphToTuple (SyntaxGraph a b c d e) = (e, b, c, d, a)
{- In the Glance image of badSyntaxGraphToTuple, it is immediately apparent that
some of the tuple arguments are out of order.


2. Reduce obfuscation:

Glance conveniently eliminates long and unnecessary binding chains. For example,
Glance simplifies the following obfuscated function

y x = g where
 c = p
 u = 2 * x
 v = c
 p = u
 g = v

to just
y x = 2 * x
-}

y x = g where
 c = p
 u = 2 * x
 v = c
 p = u
 g = v

{-
3. Make recursion visible:

Consider the recursive definition of the Fibonacci sequence

fibs = cons 0 (cons 1 (zipWith (+) fibs (tail fibs)))

In Glance, the recursive usages of fibs are explicitly shown with lines
looping from the result back into the nested icon.
-}

fibs = cons 0 (cons 1 (zipWith (+) fibs (tail fibs)))

{-
4. Simplified syntax:

In Haskell, there are many ways to write code that means the same thing.

For example, all of these lines of code mean exactly the same thing:
y = f (g x)
y = f $ g x
y = f $ g $ x
y = (f . g) x
y = f . g $ x

In Glance, these are all rendered the same.
-}
y = (f . g) x

{-
A few more examples:

notZero 0 = False
notZero _ = True

and

notZero x = case x of
  0 -> False
  _ -> True

are both rendered as
-}
notZero x = case x of
  0 -> False
  _ -> True
{-

In Haskell, guards are not expressions, so having multiple levels of guards
is quite awkward. The textual function below, for example,
requires two superfluous variables, temp1 and temp2.

In Glance, this sort of code is no problem.

foo x y
  | x > 0 = let
      temp1
        | y > 0 = 'a'
        | otherwise = 'b'
      in temp1
  | otherwise = let
      temp2
        | y > 0 = 'c'
        | otherwise = 'd'
      in temp2

-}

foo x y
  | x > 0 = let
      temp1
        | y > 0 = 'a'
        | otherwise = 'b'
      in temp1
  | otherwise = let
      temp2
        | y > 0 = 'c'
        | otherwise = 'd'
      in temp2

{-In future version of Glance, nested guards/cases could be rendered using a 2D grid.
-}
