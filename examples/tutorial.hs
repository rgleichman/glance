{-Welcome to Glance! My goal for this tutorial is teach you how to read
Glance drawings, but also to get you thinking about visual programming languages.
Why is Glance designed the way it is? What are other ways it could work?
How could it be extended? I feel that we are just at the very beginning of
visual programming languages, and that there is a huge universe of  visual
programming language designs waiting to be discovered.

Let's start with y = f x.
The function f is applied to the argument x, and the result is bound to y.
-}
y = f x

{-More explicitly-}
result = function argument

{-For example, multiply 3 and 5 and bind the result to y.
y = (*) 3 5
-}
y = (*) 3 5

{-Infix function application is displayed the same.
y = 3 * 5
-}
y = 3 * 5

{-You might be thinking that so far it looks like Glance has just decorated
the program text with some boxes, lines, triangles and circles. Don't worry,
it will start looking much stranger from here on. Keep in mind that the design
of the icons by themselves does not have much meaning. In fact, as long as the
icons (the non-text parts) are distinguishable from each other, how they are drawn,
even if they are rotated, reflected, squashed or squished does not matter.
The meaning of Glance is entirely in how the icons are connected to each other.
Lines in Glance are largely equivalent to names (variable names, function names,
etc.) in textual Haskell. Thus if two things are connected with a line in Glance,
it means they have the same value. Line colors are randomized.

Let's make things a bit more interesting with a nested function application.
y = (*) ((+) 8 7) 2
-}
y = (*) ((+) 8 7) 2

{-That was probably not too surprising. How about this?-}
f x = 3 * x

{- Let's try to figure it out. First look at the f. It's green and in an orange
box which indicates that it is bound to the result. Now look at the
function application. It looks like 3 is being multiplied by something, but what?
There is a line connecting the second argument of * to the dot in the green icon,
so the second line probably originates from the dot in the green icon.
The result of the function application is also connected to the blue square
in the green icon. So the green icon represents a relationship between the
overall result f, the second argument to *, and the result of the multiplication.
Let's think of some possibilities.
Might it be
f = 3 * f?
Let's see what that looks like:
-}
f = 3 * f

{-Nope, no green icon. The recursion of f is just indicated by a line from f to
the argument. What construct in Haskell has three ingredients: a name, a variable,
and a result that derives from the variable?  Here's a hint.
y has a value of 21.
-}
y = (\x -> 3 * x) 7

{-Now for the answer. As you might have suspected. The green icon defines a
function. The dot inside the icon is the formal parameter. The blue square
represents the value returned by function (i.e. what's on the right size
of the -> in a lambda expression). The green circle represents the function
that has been defined.

In this case, the formal parameter x is the dot inside the green lambda icon,
and the return value 3 * x is the red circle in the function application icon,
which is connected to the blue square. The function itself is bound to the name f.

f = (\x -> 3 * x)
-}
f = (\x -> 3 * x)

{-
Here, the new function is applied to the argument 7, and the result of the
function application is bound to y.
y = (\x -> 3 * x) 7
-}
y = (\x -> 3 * x) 7

{-A more complex example:
-f x y =  let q = ((f (x + y - 40) x) * 2) in
--  max (2 * y) q
If you have some drawing implements handy, you might want to try drawing this
yourself before scrolling down. One place to start is to figure out how functions
with "multiple parameters" are defined (which is of course syntactic sugar since
Haskell functions only have one parameter).
Scroll down for the drawing.
-
-
-
-
-
-
-
-
-
-
-
-
-
-
-
-
-
-
-
-
-
-
-
-
-
-
-
-
-
Drawing below:
-
-
-
-
-
-
-
-
-
-f x y =  let q = ((f (x + y - 40) x) * 2) in
--  max (2 * y) q
-}
f x y =  let q = ((f (x + y - 40) x) * 2) in
  max (2 * y) q

{-Something different about Glance is that Glance does not have any code regions.
In other visual programming languages, the icons inside the body of a function
would be restricted to a rectangle. In Glance, all icons are on the same level.
Theoretically, Glance's flat layout should allow more compact drawings since space
is not wasted by extra boxes.

f1 = (\x1 -> (\x2 -> (\x3 -> sum [x1, x2, x3])))
-}
f1 = (\x1 -> (\x2 -> (\x3 -> sum [x1, x2, x3])))

{-In most other visual languages, the above code would require three nested
regions.

Regions however can still be useful to tell at what level, or in other words, in
which function a parameter is used. In the code below for example, it would
probably take some time to figure out that x is only being used in an inner
function. To address this, I hope to have Glance draw a perimeter around all
icons inside a function (including the function's lambda icon). This would occur
after layout so it would not make the drawing any larger.

f1 x y = (\z -> x + z) y
-}
f1 x y = (\z -> x + z) y

{-Lets go back to the function apply icon. If you are used to other graphical
visual programming languages, you may be thinking that this does not look very
graphical. Here graphical is referring to a graph topology,
and no it does not look graphical. The core idea is that nested function
application has a tree topology, not a graph topology. The result of each
sub-expression is only used once. Glance takes advantage of this tree topology
to make its drawings more compact.

y = foo (3 + (baz 2)) (8* (baz 2))
-}
y = foo (3 + (baz 2)) (8* (baz 2))

{-As soon as an expression is used more than once, the tree topology is lost,
and Glance extracts the sub-expression into a separate (non-nested) icon.

y = foo (3 + bazOf2) (8* bazOf2) where bazOf2 = baz 2
-}
y = foo (3 + bazOf2) (8* bazOf2) where bazOf2 = baz 2

{-There are many different ways that function application trees can be represented.
The linear layout Glance currently uses is just the simplest. Large expressions
(just like long lines of code) become hard to read with the linear layout.
Other tree layouts could make these large expressions much more readable.

y = (((2 + 4 * 4) - (7+ 2 + baz)*8)/21)
-}
y = (((2 + 4 * 4) - (7+ 2 + baz)*8)/21)

{-Continuing on, here is a simple pattern match.
(Just x) = Just 3
-}
(Just x) = Just 3

{-
Since constructors are functions, the match icon has a same shape similar
to the apply icon. The match icon is magenta to help distinguish
it from the apply icon. In a future iteration of the Glance icons, the match and
apply icons should be made more dissimilar so that they can not be confused with
each other, even when displaying Glance drawings in black and white.

Now that you are familiar with matches, here's a simple case expression.
--y = case maybeInt of
--  Just x -> x + 1
--  Nothing -> 0
-}
y = case maybeInt of
  Just x -> x + 1
  Nothing -> 0

{-The case icon is the magenta icon with three yellow circles next to it.
The matches (the textual part left of the arrow) connect to the triangles,
and the result for each match (the textual part to the right of the arrow) is
indicated with the yellow circle. The result for the second match (Nothing -> 0)
connects to the circle on the case icon since the right hand side (0) does not
use any value from the pattern (Nothing). For the first match, since the result
(x + 1) is topologically connected to its pattern (Just x), Glance can connect
the result to a new yellow circle. If the result always had to be connected to
the yellow result circle on the case icon, this would create many cycles in the
graph, making the layout much messier.

On a side note, the lambda icon also creates a cycle, but it only creates one cycle
as opposed to the case icon which would create many cycles if remote result circles
were not allowed.

Guards and if expressions look like this:
--y | x == 0 = 1
---  | otherwise = x + 1
-}
y | x == 0 = 1
  | otherwise = x + 1

{-The Boolean expressions (e.g. x == 0) connect to the orange Ls, and the
corresponding result expressions (e.g. x + 1) connect to the triangle on the
other side of the mid-line. The overall result that the guard is bound to connects
to either the top or bottom of the mid-line.

Currently, the guard icon and the case icon look similar since they have similar
topology (number of connections = 1 + 2 * n), but they should look less similar
in better icon versions.

"If" expressions are rendered the same as a guard with only one Boolean.

--factorial x =
--  if x == 0
--    then 1
--    else factorial (x - 1) * x
-}
factorial x =
  if x == 0
    then 1
    else factorial (x - 1) * x

{- Bonus section:

The depth of an icon's application tree is called the nesting depth.
For example, The icon representing "factorial (x - 1) * x" above has a nesting
depth of 3.

To reduce nesting depth, Glance has an icon that represents an argument applied
to a composition of functions.

For example:
y = f (g x)
-}
y = f (g x)

{-
Glance figures out automatically when to use the compose icon in order to
reduce the nesting depth.

For example, if we slightly rewrite the factorial function above, Glance
uses a compose icon for the else expression.

To enable the compose icon, we change the expression
factorial (x - 1) * x
to
x * factorial (x - 1)

Notice that the nesting level has been reduced from 3 to 2.

--factorial x =
--  if x == 0
--    then 1
--    else x * factorial (x - 1)
-}
factorial x =
  if x == 0
    then 1
    else x * factorial (x - 1)
