{-Welcome to Glance! My goal for this tutorial is teach you how to read
Glance drawings, but also to get you thinking about visual programming languages.
Why is Glance designed the way it is? What are other ways it could work?
How could it be extended? I feel that we are just at the very beginning of
visual programming languages, and that there is a huge universe of visual
programming language designs waiting to be discovered.

This tutorial assumes that the reader has some familiarity with the basics
of Haskell.


Function Application:

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
of the icons does not have much meaning. In fact, as long as the
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

{-

The Mystery Icon:

That was probably not too surprising. How about this? What does the green icon
represent?
-}
f x = 3 * x

{-Let's try to figure it out. First look at the f. It's green and in an orange
box which indicates that it is bound to the result. Now look at the
function application. It looks like 3 is being multiplied by something, but what?
There is a line connecting the second argument of * to the dot in the green icon,
so the second argument probably comes from the dot in the green icon.
The result of the function application is also connected to the blue square
in the green icon. So the green icon represents a relationship between the
overall result f, the second argument to *, and the result of the multiplication.

What construct in Haskell has three ingredients: a name, a variable,
and a result that derives from the variable?  Here's a hint.
y has a value of 21.
-}
y = (\x -> 3 * x) 7

{-Now for the answer. As you might have suspected. The green icon defines a
function. The dot inside the icon is the formal parameter. The blue square
represents the value returned by function (i.e. what's on the right side
of the -> in a lambda expression). The green circle represents the function
that has been defined.

A dashed boundary is drawn around all the icons inside a function, including the
function definition icon itself. This makes it easier to tell what icons belong
to which function definition.

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
f x y =  max (2 * y) (1 + x)

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
f x y =  max (2 * y) (1 + x)
-}
f x y =  max (2 * y) (1 + x)

{-
Here are two examples of nested functions:

f1 = (\x1 -> (\x2 -> (\x3 -> sum [x1, x2, x3])))
-}
f1 = (\x1 -> (\x2 -> (\x3 -> sum [x1, x2, x3])))

{-
f1 x y = (\z -> x + z) y
-}
f1 x y = (\z -> x + z) y

{-

Graph and Tree Topology:

Let's go back to the function apply icon. If you are used to other graphical
programming languages, you may be thinking that the drawing below
does not look very graphical. Here graphical is referring to a graph topology,
and no it does not look graphical. The core idea is that nested function
application has a tree topology, not a graph topology. The result of each
sub-expression is only used once. Glance takes advantage of this tree topology
to make its drawings more compact.

y = foo (3 + (baz 2)) (8 * (baz 2))
-}
y = foo (3 + (baz 2)) (8 * (baz 2))

{-As soon as an expression is used more than once, the tree topology is lost,
and Glance extracts the sub-expression into a separate (non-nested) icon.

y = foo (3 + bazOf2) (8 * bazOf2) where bazOf2 = baz 2
-}
y = foo (3 + bazOf2) (8 * bazOf2) where bazOf2 = baz 2

{-There are many different ways that function application trees can be represented.
The linear layout Glance currently uses is just the simplest. Large expressions
(just like long lines of code) become hard to read with the linear layout.
Other tree layouts could make these large expressions much more readable.

y = (((2 + 4 * 4) - (7 + 2 + baz) * 8) / 21)
-}
y = (((2 + 4 * 4) - (7 + 2 + baz) * 8) / 21)

{-
Patterns:

Continuing on, here is a simple pattern match.
(Just x) = Just 3
-}
(Just x) = Just 3

{-
Since data constructors are functions, the match icon has a topology similar to
the apply icon.

Now that you are familiar with matches, here's a simple case expression.
y = case maybeInt of
  Nothing -> 0
  Just x -> x + 1
-}
y = case maybeInt of
  Nothing -> 0
  Just x -> x + 1

{-The case icon is the magenta icon with three yellow circles next to it.
The patterns connect to the triangles, and the result for each match is indicated
with the yellow circle.

The result for the first match (Nothing -> 0)
connects to the circle on the case icon since the right hand side (0) does not
use any value from the pattern (Nothing). For the second match, since the result
(x + 1) is connected to its pattern (Just x), Glance can connect
the result of (x + 1) to a new yellow circle.


Guards:

Guards and if expressions look like this:
y | x == 0 = 1
  | otherwise = x + 1
-}
y | x == 0 = 1
  | otherwise = x + 1

{-The Boolean expressions (e.g. x == 0) connect to the orange Ls, and the
corresponding result expressions (e.g. x + 1) connect to the triangles on the
other side of the mid-line. The overall result that the guard is bound to connects
to the bottom of the mid-line.

Currently, the guard icon and the case icon look similar since they have similar
topology, but they should look less similar in better icon versions.

"If" expressions are rendered the same as a guard with only one Boolean.

factorial x =
  if x == 0
    then 1
    else factorial (x - 1) * x
-}
factorial x =
  if x == 0
    then 1
    else factorial (x - 1) * x

{-
Compose (bonus section):

The depth of an icon's application tree is called the nesting depth.
For example, The icon representing "factorial (x - 1) * x" above has a nesting
depth of 3, since it is an apply icon (depth=1), inside an apply icon (depth=2),
inside an apply icon (depth=3).

To reduce nesting depth, Glance has an icon that represents an argument applied
to a composition of functions.

For example:
y = f (g x)
which is the same as
y = (f . g) x
-}
y = f (g x)

{-
With a composition of three functions:
y = f (g (h x))
which is the same as
y = (f . g . h) x
-}
y = f (g (h x))

{-
Glance figures out automatically when to use the compose icon in order to
reduce the nesting depth.

For example, if we slightly rewrite the factorial function above, Glance
uses a compose icon for the else expression.

To enable the compose icon, we change the expression
factorial (x - 1) * x
to
x * factorial (x - 1)

Glance essentially rewrites the second expression as:
(x *) . factorial . (x -) $ 1

Notice that the nesting depth has been reduced from 3 to 2.

factorial x =
  if x == 0
    then 1
    else x * factorial (x - 1)
-}
factorial x =
  if x == 0
    then 1
    else x * factorial (x - 1)
