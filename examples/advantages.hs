{-Advantages of a Visual Representation of Code:

The following are several admittedly contrived examples demonstrating six
of the advantages of displaying code visually instead of textually.


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
{-In the Glance image of badSyntaxGraphToTuple, it is immediately apparent that
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


5. Information at a Glance:

Let's look again at the factorial function from the README.

factorial x =
  if x == 0
    then 1
    else x * factorial (x - 1)
-}
factorial x =
  if x == 0
    then 1
    else x * factorial (x - 1)
{-In the visual version, it is quick and easy to see that the parameter (x)
is used exactly three times in the body of the function. In the text,
figuring out how many times a parameter is used requires searching through
the entire function.

Similarly, in the visual image, it is easy to see that "factorial" is recursively
called exactly once.


6. See the Topology of Code:

Assume we have a function "mapFirst" that maps the first item in a tuple.

mapFirst :: (a -> a) -> (a, a) -> (a, a)
mapFirst f (x, y) = (x', y) where
  x' = f x
-}
mapFirst f (x, y) = (x', y) where
  x' = f x

{-Now, for whatever reason, we want to write a version of mapFirst where the items
in the result tuple are swapped like so:
mapFirstAndSwap f (x, y) = (y, x') where
  x' = f x

But we make a mistake and accidentally write:
badMapFirstAndSwap f (x, y) = (x', x') where
  x' = f x

When we visualize badMapFirstAndSwap we can immediately see that (f x)
is no longer nested, and has become an additional node in the graph.

Since swapping the values in a tuple should not change the topology of our code,
this extra node indicates there is an error in the code.
-}
badMapFirstAndSwap f (x, y) = (x', x') where
  x' = f x

{-
If you were curious, here is what the good version of mapFirstAndSwap
looks like.
-}
mapFirstAndSwap f (x, y) = (y, x') where
  x' = f x
