{-Welcome to Glance! My goal for this tutorial is teach you how to read
Glance drawings, but also to get you thinking about visual programming languages.
Why is Glance designed the way it is? What are other ways it could work?
How could it be extended? I feel that we are just at the very beginning of
visual programming languages, and that there is a huge universe of  visual
programming language designs waiting to be discovered.

Let's start with y = f x.
The function f is applied to the argument x, and the result is  bound to y.
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
it means they have the same value.

Let's make things a bit more interesing with a nested function application.
y = (*) ((+) 8 7) 2
-}
y = (*) ((+) 8 7) 2

{-That was probably not too surprising. How about this?-}
f x = 3 * x

{- Let's try to figure it out. First look at the f. It's green and in an orange
box which indicates that it is bound to the result. Now look at the
function application. It looks like 3 is being multipled by something, but what?
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
y has a value of 14.
-}
y = (\x -> 3 * x) 7

{-Now for the answer. As you might have suspected. The green icon defines a
function. The dot inside the icon is the formal parameter. The blue square
represents the value returned by function (ie. what's on the right size
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
y = (\x -> 3 * x) 7-
-}
y = (\x -> 3 * x) 7

{-A more complex example:
f x y =  max (2 * y) (f (x + y - 40) x*2 )
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
f x y =  max (2 * y) (f (x + y - 40) x*2 )
-}
f x y =  max (2 * y) (f (x + y - 40) x*2 )

{-Something different about Glance is that Glance does not have any code regions
where, for example, the icons composing the body of a function would be restricted
to a rectangle. In Glance, all icons are on the same level. Theoretically,
Glance's flat layout should allow more compact drawings since space is not wasted
by extra boxes.

f1 = (\x1 -> (\x2 -> (\x3 -> sum [x1, x2, x3])))
-}
f1 = (\x1 -> (\x2 -> (\x3 -> sum [x1, x2, x3])))

{-In most other visual languages, the above code would require three nested
regions.

Regions however can still be useful to tell at what level, or in other words, in
which function is a parameter used. In the code below for example, it would
probably take some time to figure out that x is only being used in an inner
function. To address this, I hope to have Glance draw a perimiter around all
icons inside a function (including the function's lambda icon). This would occur
after layout so it would not make the drawing any bigger.

f1 x y = (\z -> x + z) y
-}
f1 x y = (\z -> x + z) y
